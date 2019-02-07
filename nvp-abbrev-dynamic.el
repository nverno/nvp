;;; nvp-abbrev-dynamic.el --- Generate dynamic abbrev tables -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-07 04:46:51>
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
(require 'nvp)


;; default method for elisp buffers/files
;;;###autoload
(cl-defgeneric nvp-abbrev-dynamic (&optional buffer-or-file append)
  "Default method to create dynamic abbrevs from BUFFER-OR-FILE.
If APPEND is non-nil, add abbrevs to current dynamic table."
  (interactive
   (list
    (pcase (car current-prefix-arg)
      (4 nil)
      (16 (let ((lib (call-interactively #'locate-library)))
            (setq lib (concat (file-name-sans-extension lib) ".el"))
            (if (file-exists-p lib) lib
              (concat lib ".gz"))))
      (_ (buffer-file-name)))))
  (let ((fns (or (and file (nvp-elisp--file-functions file))
                 (nvp-elisp--buffer-functions))))
    (when fns
      (when (bound-and-true-p nvp-elisp-abbrev-table)
        (clear-abbrev-table nvp-elisp-abbrev-table))
      (define-abbrev-table 'nvp-elisp-abbrev-table
          (nvp-elisp--make-abbrevs :objects fns)
        :parents (list emacs-lisp-mode-abbrev-table)
        :regexp nvp-lisp-abbrev-re
        :enable-function 'nvp-elisp-abbrev-expand-p)
      (setq-local local-abbrev-table nvp-elisp-abbrev-table))))

;; Create abbrevs from obarray/list/symbol/string
;; MIN-LENGTH determines the cutoff length for objects to consider for abbrevs
;; PREDICATE is a function called with one arg, the candidate, returning non-nil
;; if the candidate should be considered as an abbrev.
;; TRANSFORMER is a function called with one arg, the candidate, returning
;; the abbreviated value to use for expansion
(cl-defgeneric nvp-abbrev--make-abbrevs (&key objects min-length predicate
                                             transformer)
  "Default function to convert OBJECTS into abbrevs.")

(provide 'nvp-abbrev-dynamic)
;;; nvp-abbrev-dynamic.el ends here
