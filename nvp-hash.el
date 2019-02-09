;;; nvp-hash.el --- hash table -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-01 19:35:03>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Maintainer: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created:  1 February 2019

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
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