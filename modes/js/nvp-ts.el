;;; nvp-ts.el --- typescript -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'typescript-mode)
(require 'web-mode)
(nvp:decls)

(cl-defmethod nvp-newline-dwim-comment
  (syntax arg &context (major-mode typescript-mode))
  (nvp-newline-dwim--comment syntax arg " * "))

(cl-defmethod nvp-newline-dwim-comment
  (syntax arg &context (major-mode typescript-tsx-mode))
  (nvp-newline-dwim--comment syntax arg " * "))

;; -------------------------------------------------------------------
;;; Tsx
;; @see https://github.com/syl20bnr/spacemacs/blob/develop/layers/%2Blang/typescript/packages.el

;;;###autoload
(define-derived-mode typescript-tsx-mode web-mode "Tsx")

(provide 'nvp-ts)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-ts.el ends here
