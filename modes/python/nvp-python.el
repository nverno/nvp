;;; nvp-python.el --- python helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; TODO:
;; - remove tag-utils
;; - remove newline
;; - remove pyenv stuff
;; - generics

;;; Code:
(eval-when-compile
  (require 'nvp-macro))
(require 'python)
(require 'tag-utils nil t)
(nvp-decl "pyenv" pyenv-mode-versions pyenv-mode-unset pyenv-mode-set pyenv-mode)
(nvp-decl anaconda-mode-complete-extract-names anaconda-mode-call)
(nvp-decls)

;;; Variables

(defvar nvp-python-cython-repo "https://github.com/python/cpython")

;; -------------------------------------------------------------------
;;; Util

(cl-defmethod nvp-parse-current-function
  (&context (major-mode python-mode) &rest _args)
  (add-log-current-defun))

;; goto beginning of current class if in one
(defun nvp-python-beginning-of-class ()
  (let ((start (point)))
    (while (not (or (bobp) (bolp) (looking-at-p "class")))
      (python-nav-backward-block))
    (unless (looking-at-p "class")
      (goto-char start))))

;; name of current class and super class if exists
(defun nvp-python-current-class ()
  (save-excursion
    (nvp-python-beginning-of-class)
    (and (looking-at (nvp-concat "class +\\([A-Z][A-Za-z0-9_]*\\)"
                                 "\\(?:(\\([^)]+\\))\\)?"))
         (cons (match-string 1) (match-string 2)))))

;; ;; complete for current class methods
(defun nvp-python-class-candidates ()
  (interactive)
  (let ((super (cdr (nvp-python-current-class)))
        (start (point)))
    (when super
      (let* ((bnds (bounds-of-thing-at-point 'symbol))
             (tmp (if bnds (car bnds) (point))))
        (goto-char tmp)
        (insert (concat super "."))
        (goto-char (+ 1 start (length super)))
        (anaconda-mode-call
         "complete"
         (lambda (result)
           (let ((company-candidates (anaconda-mode-complete-extract-names result)))
             (delete-region tmp (+ 1 tmp (length super)))
             (company-complete))))))))

(defun nvp-python-class-complete ()
  (interactive)
  (let ((completion-at-point-functions '(nvp-python-class-candidates))
        (company-backends '(company-capf)))
    (company-complete)))

;; -------------------------------------------------------------------
;;; Tags

;;; TODO:
(defun nvp-python-tag-source ()
  (interactive))

;; -------------------------------------------------------------------
;;; Environment

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
          (nconc programs ()))))

;;; Pyenv

;; Set pyenv version from ".python-version" by looking in parent directories.
(defun nvp-python-pyenv-set-local-version ()
  (interactive)
  (let ((root-path (locate-dominating-file default-directory ".python-version")))
    (when root-path
      (let* ((file-path (expand-file-name ".python-version" root-path))
             (version (with-temp-buffer
                        (insert-file-contents-literally file-path)
                        (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position)))))
        (if (member version (pyenv-mode-versions))
            (pyenv-mode-set version)
          (message "pyenv: version `%s' is not installed (set by %s)"
                   version file-path))))))

;; Set pyenv version matching project name.
(defun nvp-python-projectile-pyenv-mode-set ()
  (let ((project (projectile-project-name)))
    (if (member project (pyenv-mode-versions))
        (pyenv-mode-set project)
      (pyenv-mode-unset))))

(with-eval-after-load 'projectile-mode
  (when (and nil (fboundp 'pyenv-mode) (pyenv-mode))
    (add-hook 'projectile-after-switch-project-hook
              #'nvp-python-projectile-pyenv-mode-set)))

;; -------------------------------------------------------------------
;;; Encoding

(defun nvp-python-cleanup-buffer ()
  (set-buffer-file-coding-system 'utf-8-unix))

;; from prelude to guess encoding
(defun nvp-python-encoding-comment-required-p ()
  (re-search-forward "[^\0-\177]" nil t))

(defun nvp-python-detect-encoding ()
  (let ((coding-system
	 (or save-buffer-coding-system
	     buffer-file-coding-system)))
    (if coding-system
	(symbol-name
	 (or (coding-system-get coding-system 'mime-charset)
	     (coding-system-change-eol-conversion coding-system nil)))
      "ascii-8bit")))

(defun nvp-python-insert-coding-comment (encoding)
  (let ((newlines (if (looking-at "^\\s *$") "\n" "\n\n")))
    (insert (format "# coding: %s" encoding) newlines)))

;; Insert a magic comment header with the proper encoding if necessary.
(defun nvp-python-set-encoding ()
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (nvp-python-encoding-comment-required-p)
      (goto-char (point-min))
      (let ((coding-system (nvp-python-detect-encoding)))
	(when coding-system
	  (if (looking-at "^#!") (beginning-of-line 2))
	  (cond ((looking-at 
		  "\\s *.*\\(en\\)?coding\\s *:\\s *\\([-a-z0-9_]*\\)")
		 ;; update existing encoding comment if necessary
		 (unless (string= (match-string 2) coding-system)
		   (goto-char (match-beginning 2))
		   (insert coding-system)))
		((looking-at "\\s *#.*coding\\s *[:=]"))
		(t (nvp-python-insert-coding-comment coding-system)))
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

;;; Compile

(defun nvp-python-basic-compile ()
  (interactive)
  (setq-local compilation-read-command nil)
  (let ((compile-command
         (format "python %s" (file-name-directory buffer-file-name))))
    (call-interactively 'compile)))

;;; FIXME: update to new advice system
;; Mastering emacs
;; Advises `compile' so it sets the argument COMINT to t
;; if breakpoints are present in `python-mode' files
(defadvice compile (before ad-compile-smart activate)
  (when (derived-mode-p major-mode 'python-mode)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (if (re-search-forward
             (concat "^\\s-*" nvp-python-debug-breakpoint-string "$")
             (point-max) t)
            ;; set COMINT argument to `t'.
            (ad-set-arg 1 t))))))

;; -------------------------------------------------------------------
;;; REPL

(eval-when-compile
  (defvar comint-input-ring-file-name)
  (defvar comint-input-ring-size))

(declare-function comint-read-input-ring "comint")
(nvp-declare "" nvp-he-history-setup conda-env)

;; mark current statement, leaves point at end of statement
(defun nvp-python-mark-current-statement ()
  (set-mark (progn (python-nav-beginning-of-statement) (point)))
  (python-nav-end-of-statement))

;; bounds of current python statement
(defun nvp-python-statement-bounds ()
  (cons (python-nav-beginning-of-statement)
        (python-nav-end-of-statement)))

(defun nvp-python-send-line-dwim (arg)
  "Send current statement and step -- current statement is that containing the
point or the statement directly preceding the point if the point isn't at 
the beginning of the line. With prefix, add a print statement and pop to 
the console."  
  (interactive "P")
  (and (python-info-current-line-comment-p)
       (forward-comment (point-max)))
  (let ((bnds (nvp-python-statement-bounds)))
    (when (and (not (bolp))                   ; maybe directly after statement
               (equal (car bnds) (cdr bnds))) ; and not in a statement
      (setq bnds (progn (beginning-of-line)   ;so, try from beginning of line
                        (nvp-python-statement-bounds))))
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
  (let* ((bnds (nvp-python-statement-bounds))
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

;; switch betweeen source and REPL buffers
(nvp-repl-switch "python" (:repl-mode 'inferior-python-mode
                           :repl-find-fn 'python-shell-get-buffer)
  ;; starts a new REPL if there isn't one running
  (call-interactively 'conda-env-send-buffer))

;; -------------------------------------------------------------------
;;; Find

(with-eval-after-load 'anaconda-mode
  (advice-add 'anaconda-mode-find-definitions :before 'xref-push-marker-stack))

;; -------------------------------------------------------------------
;;; Minor Mode

(eval-when-compile
  (defvar nvp-python-test-keymap)
  (defvar nvp-python-debug-keymap))
(define-prefix-command 'nvp-python-test-keymap nil "Test")
(define-prefix-command 'nvp-python-debug-keymap nil "Debug")

(defvar nvp-python-menu
  '("PU"
    ("Run"
     ["Compile" nvp-python-basic-compile t]
     ["REPL" nvp-python-repl-switch t])
    "--"
    ["Tag Source" nvp-python-tag-source]
    ["Install / Upgrade" nvp-python-install]))

(defvar nvp-python-mode-map
  (let ((km (make-sparse-keymap)))
    (easy-menu-define nil km nil nvp-python-menu)
    (define-key km (kbd "C-c C-z")  'nvp-python-repl-switch)
    (define-key km (kbd "<f5>")     'nvp-python-basic-compile)
    (define-key km (kbd "C-<f5>")   'nvp-python-toggle-breakpoint)
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
  (define-key pmap (kbd "C-b")      'nvp-python-debug-toggle-breakpoint)
  (define-key pmap "a"              'nosestest-pdb-all)
  (define-key pmap "m"              'nosestest-pdb-module)
  (define-key pmap "s"              'nosestest-pdb-suite)
  (define-key pmap "c"              'nosestest-pdb-one)
  (define-key pmap "h"              'nvp-pdb-hydra/body)
  (define-key pmap (kbd "<f2> m z") 'nvp-gud-pdb-repl-switch)
  (define-key pmap "H"              'nvp-python-debug-annotate-pdb))

(easy-menu-define nil nvp-python-mode-map nil
  `(,@nvp-python-menu
    "--"
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
     ["toggle pdb breakpoint" nvp-python-debug-toggle-breakpoint t]
     ["highlight traces" nvp-python-debug-annotate-pdb t]
     ["pdb hydra" nvp-pdb-hydra/body]
     "--"
     ("Nose"
      ["run all nosetests" nosetests-pdb-all t]
      ["run module tests" nosetests-pdb-module t]
      ["run test suite" nosetests-pdb-suite t]
      ["run this test" nosetest-pdb-one t]))))

;;;###autoload
(define-minor-mode nvp-python-mode "Python utils"
  nil
  :keymap nvp-python-mode-map
  :lighter " PU"
  (add-hook 'after-save-hook 'nvp-python-set-encoding nil t))

;;;###autoload (add-hook 'python-mode-hook 'nvp-python-mode)

;; -------------------------------------------------------------------
;;; Info
(declare-function info-lookup-add-help "info-look")
(with-eval-after-load 'info-look
  (info-lookup-add-help
   :mode 'python-mode
   :regexp "[a-zA-Z_0-9.]+"
   :doc-spec
   '(("(python)Python Module Index" )
     ("(python)Index"
      (lambda
        (item)
        (cond
         ((string-match
           "\\([A-Za-z0-9_]+\\)() (in module \\([A-Za-z0-9_.]+\\))" item)
          (format "%s.%s" (match-string 2 item)
                  (match-string 1 item)))))))))

(provide 'nvp-python)
;;; nvp-python.el ends here
