;;; nvp-awk.el --- awk extensions -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-04-09.23>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 26 February 2019

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro)
  (require 'nvp-font))
(require 'nvp)

;; -------------------------------------------------------------------
;;; Syntax

(modify-syntax-entry ?@ "'" (syntax-table))

;; -------------------------------------------------------------------
;;; Font-locking

(nvp-font-lock-add-defaults 'awk-mode
  ("\$\\([0-9]+\\)" (1 font-lock-variable-name-face prepend))
  (`,(concat "\\<" (regexp-opt '("BEGINFILE" "ENDFILE") t) "\\>")
   (1 font-lock-keyword-face))
  ("\\(@[[:alpha:]_]+\\)" (1 'nvp-italic-variable-face)))

;; -------------------------------------------------------------------
;;; Completion
;; PROCINFO['identifiers'], FUNCTAB

(provide 'nvp-awk)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-awk.el ends here
