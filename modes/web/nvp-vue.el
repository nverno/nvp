;;; nvp-vue.el --- vue development -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; The old `vue-mode' available on melpa doesn't add anything more than `web-mode'.
;; This just provides simple mode for abbrev/snippet control mostly.
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp-decls)

;;;###autoload
(define-derived-mode vue-mode web-mode "Vue"
  nil
  ;; :lighter " Vue"
  (yas-activate-extra-mode 'vue-mode))

(provide 'nvp-vue)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-vue.el ends here
