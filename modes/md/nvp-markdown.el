;;; nvp-markdown.el --- markdown helpers  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-08 03:21:32>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp-markdown
;; Package-Requires: 
;; Created: 13 November 2016

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
  (defvar imenu-generic-expression))
(require 'nvp-md)

;; -------------------------------------------------------------------
;;; Imenu

(defvar nvp-markdown-imenu-generic-expression
  '((nil   "^\\(.*\\)[\n]=+$"   1)   ;title 
    ("fn"  "^\\[\\^\\(.*\\)\\]" 1)   
    ("h1"  "^# \\(.*\\)$"       1)   
    ("h2"  "^## \\(.*\\)$"      1)   
    ("h2-" "^\\(.*\\)[\n]-+$"   1)   
    ("h3"  "^### \\(.*\\)$"     1)   
    ("h4"  "^#### \\(.*\\)$"    1)   
    ("h5"  "^##### \\(.*\\)$"   1)   
    ("h6"  "^###### \\(.*\\)$"  1))) 

;; Transform Elisp-style code references to Markdown-style.
(defun nvp-markdown-cleanup ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "`[^\n ]+\\('\\)" nil t)
    (replace-match "`" nil nil nil 1)))

(defun nvp-markdown-stack-block (beg end)
  (interactive "r")
  (let ((str (buffer-substring-no-properties beg end)))
    (delete-region beg end)
    (insert
     "<!-- language: lang-cl -->\n\n    "
     (mapconcat #'identity (split-string str "\n" t)
      "\n    "))))

(provide 'nvp-markdown)
;;; nvp-markdown.el ends here
