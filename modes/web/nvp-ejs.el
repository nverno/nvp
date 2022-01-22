;;; nvp-ejs.el --- embedded js lodash/underscore -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp-decls)

;;;###autoload
(define-minor-mode ejs-mode "EJS minor mode"
  :lighter " EJS"
  (yas-activate-extra-mode 'ejs-mode))

(provide 'nvp-ejs)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-ejs.el ends here
