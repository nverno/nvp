;;; nvp-css.el --- css and related modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)

;; -------------------------------------------------------------------
;;; SCSS

(defun nvp-scss-compile ()
  (interactive)
  (let* ((compile-command
          (concat "sass " (buffer-file-name) " "
                  (concat (file-name-sans-extension (buffer-file-name)) ".css"))))
    (call-interactively 'nvp-compile)))

(nvp-font-lock-add-defaults 'less-css-mode
  ;; Mixins w/ parameters
  ("\\(?:[ \t{;]\\|^\\)\\(\\.[a-z_-][a-z-_0-9]*\\)[ \t]*\("
   (1 font-lock-preprocessor-face prepend)))

(provide 'nvp-css)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-css.el ends here
