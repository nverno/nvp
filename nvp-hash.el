;;; nvp-hash.el --- hash table -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-22 19:58:18>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created:  1 February 2019

;;; Commentary:

;;; FIXME: merge with cache
;; See the emacs manual for creating a hash table test
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Hash.html

;;; Code:

;; -------------------------------------------------------------------
;;; Hash tests

(defsubst case-fold-string= (a b)
  (eq t (compare-strings a nil nil b nil nil t)))

(defsubst case-fold-string-hash (a)
  (sxhash (upcase a)))

;; case-insensitive hash-table
(define-hash-table-test 'case-fold #'case-fold-string= #'case-fold-string-hash)

(provide 'nvp-hash)
;;; nvp-hash.el ends here
