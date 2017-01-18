;;; nvp-unicode ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created:  4 December 2016

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
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib)
  (defvar nvp--dir))
(autoload 'nvp-abbrev-add-parent "nvp-abbrev")
(autoload 'nvp-process-buffer "nvp")

;; add unicode abbrevs to local table
;;;###autoload
(defun nvp-unicode-load-abbrevs ()
  (interactive)
  (nvp-abbrev-add-parent
   "unicode-abbrev-table"
   (expand-file-name "unicode-abbrev-table" nvp--dir)))

;; print julia latex
(defun nvp-unicode-julia-latex ()
  (interactive)
  (and (executable-find "julia")
       (set-process-sentinel
        (start-process
         "julia" (nvp-process-buffer) "julia"
         (expand-file-name "tools/julia_latex.jl" nvp--dir))
        #'(lambda (p _m)
            (when (zerop (process-exit-status p))
              (pop-to-buffer (nvp-process-buffer)))))))

(provide 'nvp-unicode)
;;; nvp-unicode.el ends here
