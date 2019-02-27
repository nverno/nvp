;;; nvp-term.el --- term mode -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Maintainer: Noah Peart <noah.v.peart@gmail.com>
;; Last modified: <2019-01-24 18:16:34>
;; URL: https://github.com/nverno/shell-tools
;; Package-Requires: 
;; Created: 13 January 2019

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

(require 'term)

(defun nvp-term-esc ()
  (interactive)
  (term-send-raw-string "\e"))

(defun nvp-term-eof ()
  (interactive)
  (if (or (eobp)
          (and (= 1 (- (point-max) (point)))
               (= 32 (char-before))))
      (term-send-eof)
    (delete-char 1)))

(provide 'nvp-term)
;;; nvp-term.el ends here
