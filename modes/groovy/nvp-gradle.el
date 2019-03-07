;;; nvp-gradle.el --- gradle-mode ext -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-06 18:56:34>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created:  6 March 2019

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'groovy-mode)

(defvar nvp-gradle-imenu-regexp
  (list (list "Tasks" "task[ \t]+\\([a-zA-Z][a-zA-Z0-9_]*\\)" 1)))

;;;###autoload
(define-derived-mode gradle-mode groovy-mode "Gradle"
  "Major mode for editing gradle script."
  (make-local-variable 'groovy-font-lock-keywords)
  (add-to-list
   'groovy-font-lock-keywords
   '("task[ \t]+\\([a-zA-Z][a-zA-Z0-9_]*\\)" 1 font-lock-function-name-face) t)

  (setq imenu-generic-expression nvp-gradle-imenu-regexp))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gradle\\'" . gradle-mode))

(provide 'nvp-gradle)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-gradle.el ends here
