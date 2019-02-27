;;; rfc-tools.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/web-tools
;; Package-Requires: 
;; Created: 30 July 2017

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
(require 'irfc nil t)
(declare-function irfc-visit "irfc")
(declare-function irfc-render-buffer-hide-blank-line "irfc")
(declare-function irfc-head-move "irfc")

;; load RFC config prior to visiting rfs
;;;###autoload
(defun web-tools-rfc-visit (&optional rfc-number)
  (interactive)
  (irfc-visit rfc-number))

;; -------------------------------------------------------------------
;;; imenu
(eval-when-compile
  (defvar imenu-use-markers))

(defvar-local rfc-tools-imenu-table ())

;; create alist for imenu, set `imenu-create-index-function' within irfc-hook
(defun web-tools-rfc-imenu-function ()
  (or rfc-tools-imenu-table
      (let ((title-line-point (irfc-render-buffer-hide-blank-line (point-min)))
            match-list)
        ;; fill table -- see irfc-fill-tables
        (save-excursion
          (goto-char title-line-point)
          (while (setq match-list (irfc-head-move))
            (push
             (cons
              (if (and (nth 0 match-list) (nth 1 match-list))
                  (buffer-substring-no-properties (nth 0 match-list)
                                                  (nth 3 match-list))
                (buffer-substring-no-properties (nth 2 match-list)
                                                (nth 3 match-list)))
              (if imenu-use-markers (point-marker) (point)))
             rfc-tools-imenu-table)))
        (nreverse rfc-tools-imenu-table))))

(provide 'rfc-tools)
;;; rfc-tools.el ends here
