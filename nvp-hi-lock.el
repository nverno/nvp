;;; nvp-hi-lock.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-01-31 04:49:53>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Maintainer: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://
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
(require 'hi-lock)

;;;###autoload
(defun nvp-hi-lock-forward ()
  "Jump between hi-lock matches."
  (interactive)
  (goto-char
   (apply
    #'min
    (mapcar
     (lambda (pattern)
       (save-excursion
         (re-search-forward (car pattern) nil 'noerror)
         (point)))
     hi-lock-file-patterns))))

(provide 'nvp-hi-lock)
;;; nvp-hi-lock.el ends here
