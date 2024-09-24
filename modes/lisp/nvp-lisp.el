;;; nvp-lisp.el --- lisp helpers  -*- lexical-binding: t; -*-
;;; Commentary:
;; TODO:
;; - auto-abbrevs
;; - sbcl: jump to source
;; - info
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'slime nil t)
(nvp:decls :p (slime) :f (slime) :v (*slime-company--meta-request*))
(nvp:auto "nvp-elisp" 'nvp-elisp-abbrev-expand-var-p 'nvp-elisp-abbrev-expand-fn-p)

;; return list of available lisps and their arguments
(defun nvp-lisp-implementations ()
  (let ((impls
         '(("sbcl"  sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)
           ("clisp" clisp ("clisp" "-q"))
           ("lisp"  cmucl ("cmucl" "-quiet") :coding-system iso-latin-1-unix)
           ("ccl"   ccl ("ccl")))))
    (cl-loop for (name . args) in impls
             when (or (nvp:program-search name) (executable-find name))
             collect args)))

(defun nvp-lisp-start-slime ()
  (interactive)
  (if (slime-connected-p)
      (if (< (length slime-net-processes) 2)
          (slime)
        (slime-list-connections))
    (slime)))

;; -------------------------------------------------------------------
;;; REPL

(defun nvp-lisp-eval-last-sexp (&optional prefix)
  "Eval last sexp and print output in buffer with PREFIX."
  (interactive "P")
  (funcall (if prefix #'slime-pprint-eval-last-expression
             #'slime-eval-last-expression)))

(defun nvp-lisp-eval-buffer (&optional buffer-name)
  "Evaluate BUFFER-NAME using asdf:load-system for .asd file and
`slime-eval-buffer' otherwise."
  (interactive (list (buffer-name)))
  (if (not (string-match-p
            "asd" (file-name-extension (or buffer-name (buffer-name)))))
      (call-interactively #'slime-eval-buffer)
    (let ((system (file-name-sans-extension buffer-name)))
      (slime-repl-eval-string (format "(asdf:load-system \"%s\")" system)))))

(with-eval-after-load 'nvp-repl
  (nvp-repl-add '(lisp-mode)
    :name 'slime
    :modes '(slime-repl-mode)
    :live #'(lambda (p)
              (and (process-buffer p)
                   (let ((buff (slime-connection-output-buffer p)))
                     (buffer-live-p buff))))
    ;; :find-fn #'slime-connection
    :buf->proc #'slime-connection
    :proc->buf #'(lambda (_p) (slime-output-buffer t))
    :init #'(lambda (&optional _prefix)
              (save-window-excursion
                (with-current-buffer (slime-output-buffer t)
                  slime-buffer-connection)))
    ;; :eval-defun #'slime-eval-defun
    ;; :eval-region #'slime-eval-region
    :eval-sexp #'nvp-lisp-eval-last-sexp
    :send-buffer #'nvp-lisp-eval-buffer
    :help-cmd (lambda (&rest _) (slime-repl-shortcut-help))
    :pwd-cmd (lambda (&rest _) (slime-eval '(swank:default-directory)))
    :cd-cmd (lambda (dir)
              (push (slime-eval '(swank:default-directory))
                    slime-repl-directory-stack)
              (slime-set-default-directory dir))))

;; -------------------------------------------------------------------
;;; Help at point 

(defun nvp-hap-lisp (command &optional arg &rest _args)
  (cl-case command
    (thingatpt (and (slime-connected-p)
                    (slime-symbol-at-point)))
    (doc-buffer
     (unless *slime-company--meta-request*
       (list (slime-company--doc-buffer arg))))))

;; -------------------------------------------------------------------
;;; Hippie Expand

(defvar nvp-lisp-he-expand-functions
  '(nvp-try-expand-dabbrev-closest-first
    nvp-try-expand-local-abbrevs
    nvp-try-expand-flex
    yas-hippie-try-expand
    try-expand-dabbrev-all-buffers
    try-expand-dabbrev-from-kill
    try-expand-slime-fuzzy
    try-complete-file-name-partially
    try-complete-file-name))

;; pass to `nvp-he-try-expand-history-trans'
(defun nvp-lisp-hippie-expand-setup (&optional repl)
  (nvp-he-flex-lisp-setup #'slime-symbol-start-pos)
  (setq-local hippie-expand-try-functions-list nvp-lisp-he-expand-functions)
  (setq-local hippie-expand-only-buffers '(lisp-mode))
  (when repl
    (nvp-he-history-setup :history 'slime-repl-input-history
                          :bol-fn #'line-beginning-position
                          :expand-fn #'nvp-he-history-remove-trailing-paren)))

(provide 'nvp-lisp)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-lisp.el ends here
