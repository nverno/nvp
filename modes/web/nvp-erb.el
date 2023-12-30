;;; nvp-erb.el --- erb minor mode -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls)

;;;###autoload
(define-minor-mode erb-minor-mode "ERB minor mode"
  :lighter " ERB"
  (if erb-minor-mode
      (yas-activate-extra-mode 'erb-mode)
    (yas-deactivate-extra-mode 'erb-mode)))

(provide 'nvp-erb)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-erb.el ends here
