;;; nvp-m4.el --- m4 -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-26 04:57:09>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/make-tools
;; Package-Requires: 
;; Created:  8 February 2019

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
  (require 'cl-lib)
  (require 'nvp-macro))
(require 'autoconf)
(require 'm4-mode)

;; -------------------------------------------------------------------
;;; Add font-locking 

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
