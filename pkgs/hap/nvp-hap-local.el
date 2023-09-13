;;; nvp-hap-local.el --- hap backend for local help -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'nvp-hap)
(nvp:decls)

;;;###autoload
(defun nvp-hap-local (command &optional arg &rest _args)
  (cl-case command
    (thingatpt (help-at-pt-kbd-string))
    (doc-string arg)
    (doc-buffer)))

(provide 'nvp-hap-local)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-hap-local.el ends here
