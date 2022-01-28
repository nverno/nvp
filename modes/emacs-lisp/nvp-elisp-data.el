;;; nvp-elisp-data.el --- lisp-data  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'company-elisp)
(nvp:decls)

;; wrapper for company-complete in lisp-data-mode
;; Note: lisp-data-mode is parent of emacs-lisp-mode so
;; (put 'derived-parent-mode ...) results in infinite loop
;; XXX: how to add-advice on local copy of function?
(defun nvp-company-lisp-data (&rest args)
  (interactive (list 'interactive))
  (let* ((orig-fn (symbol-function 'company-elisp))
         (major-mode 'emacs-lisp-mode))
    (apply orig-fn args)))

(provide 'nvp-elisp-data)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-elisp-data.el ends here
