;;; nvp-js.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;; - REPLs => nodejs, skewer
;; - newline
;; - xrefs => tern / js2-xref
;; - help  => tern
;; - snippet stuff
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp-decls :f (nodejs-repl-switch-to-repl
               nodejs-repl-send-region nodejs-repl-send-last-expression
               skewer-eval-print-last-expression skewer-eval-last-expression
               httpd-start
               js2-display-error-list
               tern-get-docs)
           :v (nodejs-repl-process-name httpd-root httpd-port))

;; when in /* continued comments or doxygen, add comment continuation for
;; newline-dwim
(cl-defmethod nvp-newline-dwim-comment
  (syntax arg &context (major-mode js2-mode js2-jsx-mode js-mode js-jsx-mode))
  (nvp-newline-dwim--comment syntax arg " * "))

;;; httpd

(defun nvp-httpd-here ()
  (interactive)
  (setq httpd-root default-directory)
  (httpd-start))

;; -------------------------------------------------------------------
;;; REPLs

;;; Nodejs REPL
(with-eval-after-load 'nvp-repl
  (nvp-repl-add '(js2-mode js2-jsx-mode js-mode js-jsx-mode)
    :modes '(nodejs-repl-mode)
    :procname (bound-and-true-p nodejs-repl-process-name)
    :init (lambda ()
            (save-window-excursion
              (ignore-errors
                (call-interactively #'nodejs-repl-switch-to-repl))))))

(defun nvp-js-nodejs-region-or-sexp ()
  (interactive)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'nodejs-repl-send-region)
      (call-interactively 'nodejs-repl-send-last-expression))
  (forward-line))

;; nodejs-repl doesn't manage comint history files
(define-advice nodejs-repl-quit-or-cancel (:before (&rest _) "write-history")
  (comint-write-input-ring))

;;; Skewer

(defun nvp-skewer-eval-last-expression (&optional print)
  (interactive "P")
  (call-interactively 
   (if print #'skewer-eval-print-last-expression
     #'skewer-eval-last-expression)))

;; -------------------------------------------------------------------
;;; Snippet helpers
;;; FIXME: unused

(defun nvp-js-method-p ()
  (save-excursion
    (word-search-backward "function")
    (looking-back ": " (line-beginning-position))))

(defun nvp-js-function-declaration-p ()
  (save-excursion
    (word-search-backward "function")
    (looking-back "^\\s *" (line-beginning-position))))

(defun nvp-js-snippet-punctuation ()
  (if (nvp-js-method-p)
      (when (not (looking-at "[ \n\t\r]*[},]"))
        (insert ","))
    (unless (nvp-js-function-declaration-p)
      (if (looking-at "$") (insert ";")))))

(defun nvp-js-snippet-function-name ()
  (if (nvp-js-function-declaration-p) "name" ""))

;; (defvar keyword-function
;;   '(("\\(function\\)\\>" (0 (prog1 ()
;;                               (compose-region (match-beginning 1)
;;                                               (match-end 1)
;;                                               "\u0192"))))))
;; (font-lock-add-keywords 'js2-mode keyword-function)

;; -------------------------------------------------------------------
;;; Help

(defun nvp-js-help-at-point ()
  (interactive)
  (cond
   ((member 'js2-echo-error (get-text-property (point) 'cursor-sensor-functions))
    (js2-display-error-list))
   (t (tern-get-docs))))

(provide 'nvp-js)
;;; nvp-js.el ends here
