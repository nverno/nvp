;;; nvp-css.el --- css and related modes -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-06 16:28:50>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created:  6 March 2019

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'nvp)

(nvp-package-define-root :snippets t)

;; -------------------------------------------------------------------
;;; SCSS

(defun nvp-scss-compile ()
  (interactive)
  (let* ((compile-command
          (concat "sass " (buffer-file-name) " "
                  (concat (file-name-sans-extension (buffer-file-name)) ".css"))))
    (call-interactively 'nvp-compile)))


(provide 'nvp-css)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-css.el ends here
