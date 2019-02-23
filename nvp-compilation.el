;;; nvp-compilation.el --- compilation helpers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-22 18:34:09>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 11 November 2016

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro))
(require 'compile)

;; move to next warning/error 
(defun nvp-compilation-next (n)
  "Move to next warning, error, or info message."
  (interactive "p")
  (let ((compilation-skip-threshold 0))
    (compilation-next-error n)))

(defun nvp-compilation-previous (n)
  (interactive "p")
  (nvp-compilation-next (- n)))

(provide 'nvp-compilation)
;;; nvp-compilation.el ends here
