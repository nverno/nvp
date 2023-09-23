;;; nvp-python.el --- python helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;; FIXME: to get proper completion in python REPL following chained calls,
;;         `python-shell-completion-at-point' doesn't correctly skip back over
;;         '().', so the function could be advised (similar to modification for
;;         octave in nvp-octave.el
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:req 'nvp-python 'subrs)
(require 'comint)
(require 'python)
(nvp:decls :p (anaconda conda-env))

(nvp:defmethod nvp-parse-current-function ()
  :modes (python-mode python-ts-mode)
  (add-log-current-defun))

;;; Abbrevs
;; C level only calls `expand-abbrev' when preceding char is word syntax
;; so hook into `post-self-insert-hook'
(defun nvp-python-expand-after-ops-hook ()
  (and (memq (char-before (1- (point))) '(?| ?&))
       (setq this-command 'nvp-python-expand-after-ops)
       (expand-abbrev)))

(defvar nvp-python-abbrev-syntax-table
  (let ((tab (copy-syntax-table python-mode-syntax-table)))
    (modify-syntax-entry ?| "w" tab)
    (modify-syntax-entry ?& "w" tab)
    tab))

;; temporarily treat '|' and '&' as word syntax
(defun nvp-python-expand-abbrev ()
  (c-with-syntax-table nvp-python-abbrev-syntax-table
    (abbrev--default-expand)))

(defun nvp-python-abbrev-expand-p ()
  (and (or (memq this-command '(expand-abbrev nvp-python-expand-after-ops))
           (not (memq last-input-event '(?_ ?. ?- ?:))))
       (not (nvp:ppss 'soc))))

;;; Encoding

;; Insert a magic comment header with the proper encoding if necessary.
(defun nvp-python-set-encoding ()
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (nvp:python-encoding-comment-required-p)
      (goto-char (point-min))
      (let ((coding-system (nvp:python-detect-encoding)))
	(when coding-system
	  (if (looking-at "^#!") (beginning-of-line 2))
	  (cond ((looking-at 
		  "\\s *.*\\(en\\)?coding\\s *:\\s *\\([-a-z0-9_]*\\)")
		 ;; update existing encoding comment if necessary
		 (unless (string= (match-string 2) coding-system)
		   (goto-char (match-beginning 2))
		   (insert coding-system)))
		((looking-at "\\s *#.*coding\\s *[:=]"))
		(t (nvp:python-insert-coding-comment coding-system)))
	  (when (buffer-modified-p)
	    (basic-save-buffer-1)))))))

;; -------------------------------------------------------------------
;;; Imports

;; from
;; https://www.snip2code.com/Snippet/127022/Emacs-auto-remove-unused-import-statemen
;; Use Autoflake to remove unused function -
;; 'autoflake --remove-all-unused-imports -i unused_imports.py'.
(defun python-remove-unused-imports()
  (interactive)
  (if (executable-find "autoflake")
      (progn
        (shell-command (format "autoflake --remove-all-unused-imports -i %s"
                               (shell-quote-argument (buffer-file-name))))
        (revert-buffer t t t))
    (message "Error: Cannot find autoflake executable.")))

;; -------------------------------------------------------------------
;;; Compile

(defvar nvp-python-debug-breakpoint-string
  (cond ((executable-find "ipdb") "import ipdb; ipdb.set_trace()")
        ((executable-find "pudb") "import pudb; pudb.set_trace()")
        (t "import pdb; pdb.set_trace()"))
  "Breakpoint string to highlight.")

;; if debug breakpoint is detected in source, then compile in comint mode
(define-advice compile (:around (orig cmd &optional comint) "py-maybe-debug")
  (when (derived-mode-p major-mode 'python-mode)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (if (re-search-forward
             (concat "^\\s-*" nvp-python-debug-breakpoint-string "[ \t]*$") nil t)
            (funcall orig cmd t)
          (funcall orig cmd comint))))))

;; -------------------------------------------------------------------
;;; REPL

;; switch betweeen source and REPL buffers
(with-eval-after-load 'nvp-repl
  (nvp-repl-add '(python-mode python-ts-mode)
    :modes '(inferior-python-mode)
    :find-fn #'python-shell-get-process
    :init (lambda ()
            (save-window-excursion
              (call-interactively #'conda-env-send-buffer)))))

;; add regexp for errors in code sent to REPL
(defvar nvp-python-compilation-regexp
  '("^\\([^<\n]+\\) in <[^>]+>\n\\(?:\\s-*[0-9]+.*\n\\)*---> \\([0-9]+\\)" 1 2))

(defun nvp-python-send-line-dwim (arg)
  "Send current statement and step -- current statement is that containing the
point or the statement directly preceding the point if the point isn't at 
the beginning of the line. With prefix, add a print statement and pop to 
the console."  
  (interactive "P")
  (and (python-info-current-line-comment-p)
       (forward-comment (point-max)))
  (let ((bnds (nvp:python-statement-bounds)))
    (when (and (not (bolp))                   ; maybe directly after statement
               (equal (car bnds) (cdr bnds))) ; and not in a statement
      (setq bnds (progn (beginning-of-line)   ; so, try from beginning of line
                        (nvp:python-statement-bounds))))
    (if (and (not arg) (not (equal (car bnds) (cdr bnds))))
        (python-shell-send-region (car bnds) (cdr bnds))
      (save-excursion
        (goto-char (car bnds))
        (if (looking-at-p "\\s-*print")
            (python-shell-send-region (car bnds) (cdr bnds))
          (python-shell-send-string
           (concat "print(" (buffer-substring-no-properties
                             (car bnds) (cdr bnds)) ")"))
          (display-buffer (process-buffer (python-shell-get-process))))))
    (if (eobp)
        (newline-and-indent)
     (forward-line))))

;; eval last expression
(defun nvp-python-eval-last-sexp (arg)
  (interactive "P")
  (let* ((bnds (nvp:python-statement-bounds))
         (res (python-shell-send-string-no-output
               (buffer-substring-no-properties (car bnds) (cdr bnds)))))
    (when res
      (if arg
          (let ((standard-output (current-buffer)))
            (terpri)
            (princ res)
            (terpri))
        (message "%s" res)))))

;; send interrupt/kill
(defun nvp-python-interrupt-or-kill-process (arg)
  (interactive "P")
  (let ((proc (ignore-errors (python-shell-get-process-or-error))))
    (when proc
      (if arg
          (kill-process proc)
        (interrupt-process proc)))))

;; -------------------------------------------------------------------
;;; Minor mode

(eval-when-compile
  (defvar nvp-python-test-keymap)
  (defvar nvp-python-debug-keymap))
(define-prefix-command 'nvp-python-test-keymap nil "Test")
(define-prefix-command 'nvp-python-debug-keymap nil "Debug")

(defvar nvp-python-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "<f2>d C-b") 'nvp-python-toggle-breakpoint)
    (define-key km (kbd "<f2> m t") 'nvp-python-test-keymap)
    (define-key km (kbd "<f2> m d") 'nvp-python-debug-keymap)
    km))

;; tests
(let ((pmap nvp-python-test-keymap))
  (define-key pmap "a" 'nosetests-all)
  (define-key pmap "f" 'nosetests-failed)
  (define-key pmap "m" 'nosetests-module)
  (define-key pmap "s" 'nosetests-suite)
  (define-key pmap "c" 'nosetests-one))

;; debugging
(let ((pmap nvp-python-debug-keymap))
  (define-key pmap (kbd "C-b")      'nvp-python-toggle-breakpoint)
  (define-key pmap "a"              'nosestest-pdb-all)
  (define-key pmap "m"              'nosestest-pdb-module)
  (define-key pmap "s"              'nosestest-pdb-suite)
  (define-key pmap "c"              'nosestest-pdb-one)
  (define-key pmap "h"              'nvp-pdb-hydra/body)
  (define-key pmap (kbd "<f2> m z") 'nvp-gud-pdb-repl-switch)
  (define-key pmap "H"              'nvp-python-annotate-breakpoints))

(easy-menu-define nil nvp-python-mode-map nil
  `("PU"
    ("Test"
     ["run all nosetests" nosetests-all t]
     ["run all w/ --failed" nosetests-failed t]
     ["run module tests" nosetests-module t]
     ["run test suite" nosetests-suite t]
     ["run this test" nosetests-one t])
    "--"
    ("Debug"
     ["pdb" pdb t]
     ["switch to gud-pdb repl" nvp-gud-pdb-switch t]
     ["toggle pdb breakpoint" nvp-python-toggle-breakpoint t]
     ["highlight traces" nvp-python-annotate-breakpoints t]
     ["pdb hydra" nvp-pdb-hydra/body]
     "--"
     ("Nose"
      ["run all nosetests" nosetests-pdb-all t]
      ["run module tests" nosetests-pdb-module t]
      ["run test suite" nosetests-pdb-suite t]
      ["run this test" nosetest-pdb-one t]))))

;;;###autoload
(define-minor-mode nvp-python-mode "Python utils"
  :keymap nvp-python-mode-map
  :lighter " PU"
  (add-hook 'after-save-hook 'nvp-python-set-encoding nil t))

;;;###autoload (add-hook 'python-mode-hook 'nvp-python-mode)

;; -------------------------------------------------------------------
;;; Info

;;; TODO: for method lookup, need to transform symbol at point to be
;; prefixed by <module...><class>, eg.
;; "d.popleft" -> "collections.deque.popleft"
(declare-function info-lookup-add-help "info-look")
(with-eval-after-load 'info-look
  (let ((doc-spec
         ;; Note: info node will depend on python3-doc package installed
         ;; which may add the version suffix
         '(("(python3.10)Python Module Index")
           ;; ("(python3.10)Built-in Functions")
           ("(python3.10)Index"
            (lambda
              (item)
              (cond
               ;; module functions / variables
               ((string-match
                 "\\([A-Za-z0-9_]+\\)\\(?:()\\)? (in module \\([A-Za-z0-9_.]+\\))" item)
                (format "%s.%s" (match-string 2 item)
                        (match-string 1 item)))
               ;; builtins
               ((string-match
                 "built-in function; \\([A-Za-z][A-Za-z0-9]+\\)()" item)
                (match-string 1 item))
               ;; class methods
               ((string-match
                 "\\([A-Za-z0-9_]+\\)() (\\([A-Za-z0-9_.]+\\) method)" item)
                (format "%s.%s" (match-string 2 item)
                        (match-string 1 item)))))))))
    (dolist (mode '(python-mode python-ts-mode))
      (info-lookup-add-help
       :mode mode
       :regexp "[a-zA-Z_0-9.]+"
       :doc-spec doc-spec))))

;;; W32 old env. stuff
(nvp:with-w32
 ;; update exec path to include DIR
 (defun nvp-python-add-exec-path (dir)
   (let ((path (cl-remove-duplicates
                (cons dir (split-string (getenv "PATH")
                                        path-separator))
                :test (lambda (x y) (string= (downcase x)
                                        (downcase y))))))
     (setenv "PATH" (mapconcat 'identity path path-separator))
     (setq exec-path path)))

 ;; add directories containing programs to exec-path
 (defun nvp-python-add-paths (&rest programs)
   (setq programs (delq nil programs))
   (when programs
     (mapc #'(lambda (p) (nvp-python-add-exec-path (file-name-directory p)))
           (nconc programs ())))))

(provide 'nvp-python)
;;; nvp-python.el ends here
