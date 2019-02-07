;;; nvp-syntax.el --- syntax helpers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-07 04:09:37>
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
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'nvp-macro))

(declare-function company-grab-symbol "company")

(cl-defgeneric nvp-grab-previous-symbol ()
  "Default method to grab previous symbol."
  (company-grab-symbol))

(cl-defgeneric nvp-symbol-at-point ()
  "Default method to return symbol at point."
  (symbol-at-point))

(provide 'nvp-syntax)
;;; nvp-syntax.el ends here
