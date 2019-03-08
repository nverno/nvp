;;; nvp-m4.el --- m4 -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-07 20:30:15>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/make-tools
;; Created:  8 February 2019

;;; Commentary:

;; m4 add-ons to autoconf
;;
;; TODO:
;; - add string / sh font-locking? need to understand syntax better

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro)
  (require 'lisp-mode))
(require 'autoconf)
(require 'm4-mode)
(require 'nvp-autoconf)

;; -------------------------------------------------------------------
;;; Font-locking
;; add autoconf font-locking

(eval-when-compile
  (defun nvp-m4--ac-regexp (&rest names)
    (concat "\\_<\\(A[UC]_" (regexp-opt names) "\\)\\_>(\\[\\([^\]]+\\)")))

(defvar nvp-m4-def-regex)
(let-when-compile
    ((defs '("DEFUN" "DEFUN_ONCE" "ALIAS")))
  (let ((defs-re (eval-when-compile (apply #'nvp-m4--ac-regexp defs))))
    ;; font-lock functions and defins
    (defvar nvp-m4-def-regex defs-re)))

(font-lock-add-keywords
 'm4-mode
 `((,nvp-m4-def-regex
    (1 font-lock-keyword-face) (2 font-lock-function-name-face prepend))
   ,@autoconf-font-lock-keywords))

(provide 'nvp-m4)
;;; nvp-m4.el ends here
