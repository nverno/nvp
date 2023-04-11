;;; nvp-vue.el --- vue development -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; The old `vue-mode' available on melpa doesn't add anything more than `web-mode'.
;; This just provides simple mode for abbrev/snippet control mostly.
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp:decls :f (web-mode))

;;; from spacemacs
(defvar vue-imenu-expression
  '(("html" "^<template>$" 0)
    ("js" "^<script>$" 0)
    ("js" "^\\s-*\\(data\\).*()\\s-?{" 1)
    ("js" "^\\s-*\\(mounted\\).*()\\s-?{" 1)
    ("js" "^\\s-*\\(beforeMount\\).*()\\s-?{" 1)
    ("js" "^\\s-*\\(beforeDestroy\\).*()\\s-?{" 1)
    ("js" "^\\s-*\\(created\\).*()\\s-?{" 1)
    ("js" "^\\s-*\\(computed\\):\\s-?{" 1)
    ("js" "^\\s-*\\(watch\\):\\s-?{" 1)
    ("js" "^\\s-*\\(methods\\):\\s-?{" 1)
    ("js" "^\\s-*\\(props\\):\\s-?{" 1)
    ("css" "^<css>$" 0)))

;;;###autoload
(define-derived-mode vue-mode web-mode "Vue"
  ;; :lighter " Vue"
  "Major mode for editing Vue."
  (yas-activate-extra-mode 'js-mode)
  (setq imenu-generic-expression vue-imenu-expression
        imenu-create-index-function #'imenu-default-create-index-function))

(provide 'nvp-vue)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-vue.el ends here
