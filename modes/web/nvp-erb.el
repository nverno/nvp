;;; nvp-erb.el --- erb minor mode -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp-decls)

;;;###autoload
(define-minor-mode erb-mode "ERB minor mode"
  nil
  :lighter " ERB"
  (yas-activate-extra-mode 'erb-mode))

(provide 'nvp-erb)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-erb.el ends here
