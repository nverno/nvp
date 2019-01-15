;;; nvp-hook.el --- hooks -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-01-14 18:51:52>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Maintainer: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 14 January 2019
;; Version: 0.0.1
;; Keywords:

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
  (require 'time-stamp))
(declare-function 'time-stamp "time-stamp")

;;;###autoload
(defun nvp-hook-update-timestamp ()
  "Update buffer time stamps - `before-save-hook'."
  (let ((time-stamp-pattern 
         (or time-stamp-pattern
             (pcase major-mode
               (`org-mode "#\\+DATE: <%%>$")
               (_ "15/Last modified: <%%>$")))))
    (time-stamp)))

(provide 'nvp-hook)
;;; nvp-hook.el ends here
