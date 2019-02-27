;;; nvp-latex.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-09 09:05:52>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Maintainer: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/md-tools
;; Package-Requires: 
;; Created: 31 January 2019

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
(require 'latex nil t)
(require 'tex-site nil t)
(autoload 'TeX-run-BibTeX "tex-buf")
(autoload 'TeX-command "tex-buf")

(defun nvp-latex-electric-| ()
  (interactive)
  (insert "\\| \\| ") (backward-char 3))

(defun nvp-latex-run-bibtex ()
  (interactive)
  (let ((file (file-name-nondirectory
               (file-name-sans-extension
                (buffer-file-name)))))
    (TeX-run-BibTeX
     "BibTeX"
     (format "bibtex %s" file)
     file)))

(defun nvp-latex-pdflatex-compile ()
  "Compile .tex file and show .pdf file."
  (interactive)
  (save-buffer)
  (if (file-exists-p (expand-file-name "Makefile"))
      ;; (compile "make")
      (TeX-command "make" 'TeX-master-file -1)
    (TeX-command "LaTeX" 'TeX-master-file -1)))

(provide 'nvp-latex)
;;; nvp-latex.el ends here
