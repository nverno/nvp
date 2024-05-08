;;; nvp-r-repl.el --- Run R repl -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'ess-site nil t)
(require 'ess-inf nil t)
(require 'nvp-repl)
(nvp:decls :p (ess))

;;; REPL
(defun nvp-r--repl-init (&optional _prefix)
  (interactive "P")
  (save-window-excursion
    (let ((ess-dialect "R"))
      (ess-force-buffer-current nil 'force))))

;; Dont error if `add-log-current-defun-header-regexp' isnt defined,
;; eg. in `r-ts-mode'
(defvar add-log-current-defun-header-regexp)
(define-advice ess-eval-function (:around (orig &rest args) "add-log-regex")
  (let ((add-log-current-defun-header-regexp "^\\(.+\\)\\s-+<-[ \t\n]*function"))
    (apply orig args)))

(with-eval-after-load 'nvp-repl
  (nvp-repl-add '(ess-r-mode r-ts-mode)
    :name 'R
    :modes '(inferior-ess-r-mode)
    :init #'nvp-r--repl-init
    :find-fn (lambda () (-some-> ess-local-process-name get-process))
    :send-string #'ess-send-string
    :send-region (lambda (start end) (ess-send-region (nvp-repl-process) start end))
    :send-file #'ess-load-file
    :send-defun #'ess-eval-function
    :send-buffer #'ess-eval-buffer
    :send-line #'ess-eval-line
    ;; :send-sexp
    ;; :send-statement
    ;; :eval-sexp #'ess-eval-paragraph
    ;; :eval-region #'ess-eval-region
    :history-file ".Rhistory"
    :help-cmd '(:no-arg "" :with-arg "help(\"%s\")")
    :pwd-cmd "getwd()"
    :cd-cmd "setwd(\"%s\")"))

(provide 'nvp-r-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-r-repl.el ends here
