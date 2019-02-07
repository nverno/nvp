;;; nvp-abbrev-dynamic.el --- Generate dynamic abbrev tables -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-07 15:41:24>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Maintainer: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created:  7 February 2019

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

;; Generics to generate abbrevs from buffer/file contents

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))

;; transform by splitting on '-', eg.
;; 'nvp-abbrev--lisp-transformer' => 'na:lt' abbrev
(defun nvp-abbrev--lisp-transformer (sexp)
  (mapconcat (lambda (s)
               (if (string-empty-p s) ":"
                 (substring s 0 1)))
             (split-string str "-") ""))

;; default method for elisp buffers/files
(cl-defgeneric nvp-abbrev-dynamic (&optional buffer-or-file append)
  "Default method to create dynamic abbrevs from current buffer or BUFFER-OR-FILE.
If APPEND is non-nil, add abbrevs to current buffer-local dynamic table.")

;; Create abbrevs from obarray/list/symbol/string
;; MIN-LENGTH determines the cutoff length for objects to consider for abbrevs
;; PREDICATE is a function called with one arg, the candidate, returning non-nil
;; if the candidate should be considered as an abbrev.
;; TRANSFORMER is a function called with one arg, the candidate, returning
;; the abbreviated value to use for expansion
;; (cl-defgeneric nvp-abbrev--make-abbrevs (&key objects min-length predicate
;;                                              transformer)
;;   "Default function to convert OBJECTS into abbrevs.")


(provide 'nvp-abbrev-dynamic)
;;; nvp-abbrev-dynamic.el ends here
