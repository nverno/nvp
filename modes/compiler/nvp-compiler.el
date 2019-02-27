;;; nvp-compiler.el --- compiler helpers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/compiler-tools
;; Last modified: <2019-02-25 21:43:36>
;; Created: 15 November 2016

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))

(nvp-package-define-root :snippets t)

(provide 'nvp-compiler)
;;; nvp-compiler.el ends here
