;;; nvp-autos.el --- homeless autos -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-02 20:59:19>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Maintainer: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created:  2 February 2019

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
(require 'nvp)

;; -------------------------------------------------------------------
;;; Movement

;;;###autoload
(defun nvp-move-down-paragraph (&optional arg)
  (interactive "p")
  (if (bolp)
      (progn
        (forward-paragraph arg)
        (forward-line 1))
    (line-move arg)))

;;;###autoload
(defun nvp-move-up-paragraph (&optional arg)
  (interactive "p")
  (if (bolp)
      (progn
        (forward-line -1)
        (backward-paragraph arg)
        (forward-line 1))
    (line-move (- arg))))

(provide 'nvp-autos)
;;; nvp-autos.el ends here
