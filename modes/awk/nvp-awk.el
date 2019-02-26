;;; nvp-awk.el --- awk extensions -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-26 15:57:38>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 26 February 2019

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'nvp)

;; -------------------------------------------------------------------
;;; Syntax

(modify-syntax-entry ?@ "'" (syntax-table))

;; -------------------------------------------------------------------
;;; Font-locking

(font-lock-add-keywords
 'awk-mode
 '(("\$\\([0-9]+\\)" (1 font-lock-variable-name-face prepend))
   ("\\(@[[:alpha:]_]+\\)" (1 'nvp-italic-variable-face))))

;; -------------------------------------------------------------------
;;; Completion


(provide 'nvp-awk)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-awk.el ends here
