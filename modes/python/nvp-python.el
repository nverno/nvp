;;; nvp-python.el --- python helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;; FIXME: to get proper completion in python REPL following chained calls,
;;         `python-shell-completion-at-point' doesn't correctly skip back over
;;         '().', so the function could be advised (similar to modification for
;;         octave in nvp-octave.el
;;; Code:

(eval-when-compile (require 'nvp-macro))
(require 'python nil t)
(nvp:decls :p (python conda-env) :f (conda-env-send-buffer))

(nvp:defmethod nvp-parse-current-function ()
  :modes (python-mode python-ts-mode)
  (add-log-current-defun))

;; bounds of current python statement
(defsubst nvp-python-statement-bounds ()
  (cons (python-nav-beginning-of-statement) (python-nav-end-of-statement)))

(defvar nvp-python-breakpoint-string
  (cond ((executable-find "ipdb") "import ipdb; ipdb.set_trace()")
        ((executable-find "pudb") "import pudb; pudb.set_trace()")
        (t "import pdb; pdb.set_trace()"))
  "Breakpoint string to highlight.")

;;; REPL

;; switch betweeen source and REPL buffers
(with-eval-after-load 'nvp-repl
  (nvp-repl-add '(python-mode python-ts-mode)
    :name 'python
    :modes '(inferior-python-mode)
    :find-fn #'python-shell-get-process
    :send-string #'python-shell-send-string
    :send-region #'python-shell-send-region
    :send-file #'python-shell-send-file
    :eval-string #'python-shell-send-string-no-output
    :eval-sexp #'nvp-python-eval-last-sexp
    :cd-cmd "import os; os.chdir(\"%s\")"
    :pwd-cmd "import os; os.getcwd()"
    :help-cmd '(:no-arg "help()" :with-arg "help(%s)")
    :init (lambda (&optional _prefix)
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
  (let ((bnds (nvp-python-statement-bounds)))
    (when (and (not (bolp))                   ; maybe directly after statement
               (equal (car bnds) (cdr bnds))) ; and not in a statement
      (setq bnds (progn (beginning-of-line)   ; so, try from beginning of line
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
(defun nvp-python-eval-last-sexp (&optional arg)
  (interactive "P")
  (let* ((bnds (nvp-python-statement-bounds))
         (res (python-shell-send-string-no-output
               (buffer-substring-no-properties (car bnds) (cdr bnds)))))
    (when res
      (if arg
          (let ((standard-output (current-buffer)))
            (princ res))
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
    (dolist (mode '(python-mode)) ; python-ts-mode
      (info-lookup-add-help
       :mode mode
       :regexp "[a-zA-Z_0-9.]+"
       :doc-spec doc-spec))))


(provide 'nvp-python)
;;; nvp-python.el ends here
