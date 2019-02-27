;;; nvp-pdf.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/md-tools
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
  (require 'cl-lib)
  (defvar pdf-info-epdfinfo-program))
(require 'nvp-md)
(require 'pdf-tools)

;; ------------------------------------------------------------
;;; Setup

(defvar pdf-tools-auto-mode-alist-entry)
(defun nvp-pdf-setup ()
  (unless (file-exists-p pdf-info-epdfinfo-program)
    (pdf-tools-install t t t)
    ;; (call-interactively 'nvp-pdf-install-epdfinfo))
    ;; add enty that `pdf-tools-install' would add
    (add-to-list 'auto-mode-alist pdf-tools-auto-mode-alist-entry)))

(provide 'nvp-pdf)
;;; nvp-pdf.el ends here
