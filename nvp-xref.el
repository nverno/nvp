;;; nvp-xref.el --- xref -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-24 04:49:14>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created:  7 February 2019

;;; Commentary:
;; - #<marker at 84484 in etags.el.gz>
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'nvp-macro))
(require 'xref)

(defun nvp-xref-next-line ()
  (interactive)
  (xref--search-property 'xref-item))

(defun nvp-xref-prev-line ()
  (interactive)
  (xref--search-property 'xref-item t))

(provide 'nvp-xref)
;;; nvp-xref.el ends here
