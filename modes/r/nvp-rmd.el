;;; nvp-rmd.el --- Rmarkdown helpers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/r-tools
;; Last modified: <2019-01-15 18:53:25>
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
  (require 'polymode-core nil t)
  (defvar pm/chunkmode))
(declare-function pm-with-narrowed-to-span "polymode")
(declare-function pm-map-over-spans "polymode-core")
(declare-function pm-narrow-to-span "polymode-core")

(autoload 'eieio-oref "eieio-core")
(autoload 'ess-eval-region "ess")
(eieio-declare-slot :mode)

;; Send R code regions from polymode
(defun nvp-rmd-send-chunk ()
  "Send current R chunk to ess process."
  (interactive)
  (and (eq (oref pm/chunkmode :mode) 'r-mode)
       (pm-with-narrowed-to-span nil
         (goto-char (point-min))
         (forward-line)
         (ess-eval-region (point) (point-max) nil nil 'R))))

(defun nvp-rmd-send-buffer (arg)
  "Send all R code blocks in buffer to ess process. With prefix
send regions above point."
  (interactive "P")
  (save-restriction
    (widen)
    (save-excursion
      (pm-map-over-spans
       'nvp-rmd-send-chunk (point-min)
       (if arg (point) (point-max))))))

(provide 'nvp-rmd)
;;; nvp-rmd.el ends here
