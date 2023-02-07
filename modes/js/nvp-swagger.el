;;; nvp-swagger.el --- swagger -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp:decls)

;;;###autoload
(define-minor-mode swagger-mode "Swagger"
  :lighter " Swag"
  (if swagger-mode
      (yas-activate-extra-mode 'swagger-mode)
    (yas-deactivate-extra-mode 'swagger-mode)))

(provide 'nvp-swagger)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-swagger.el ends here
