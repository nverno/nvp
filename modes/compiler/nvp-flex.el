;;; nvp-flex.el --- flex helpers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/compiler-tools
;; Last modified: <2019-02-26 01:52:12>
;; Created: 17 February 2017

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'nvp-compiler)
(declare-function nvp-env-add "nvp-env")

;;; Environment

;; add flex source/headers to CPATH
(defun nvp-flex-setenv ()
  (interactive)
  (nvp-env-add "CPATH" (expand-file-name "flex/src" (getenv "DEVEL"))))

;; -------------------------------------------------------------------
;;; Commands

;; replace symbol at point with case-insensitve regex version
;; eg. thing => [Th][Hh][Ii][Nn][Gg]
(defun nvp-flex-uncasify-symbol ()
  (interactive)
  (let* ((bnds (bounds-of-thing-at-point 'symbol))
         (sym (buffer-substring (car bnds) (cdr bnds)))
         (str (mapconcat 'identity
                         (cl-mapcar (lambda (a b)
                                      (concat "[" (char-to-string a)
                                              (char-to-string b) "]"))
                                    (upcase sym) (downcase sym))
                         "")))
    (delete-region (car bnds) (cdr bnds))
    (insert str)))

(provide 'nvp-flex)
;;; nvp-flex.el ends here
