;;; nvp-cmake.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-02-27 11:21:11>
;; Created: 20 January 2017

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))

(nvp-package-define-root :snippets t)

;; Newline
(nvp-newline nvp-cmake-newline-dwim nil
  :pairs (("(" ")")))

(provide 'nvp-cmake)
;;; nvp-cmake.el ends here
