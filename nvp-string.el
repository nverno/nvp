;;; nvp-string.el --- string manipulations -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-21 02:45:27>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 21 February 2019

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
;; some functions from s.el that aren't covered in subr-x, various others
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'nvp-macro))

(defun nvp-s-all-matches (regex str &optional group)
  "Find all matches of REGEX in STR for regex GROUP (default 0)."
  (declare (side-effect-free t))
  (or group (setq group 0))
  (save-match-data
    (let ((pos 0) (len (length str)) matches)
      (while (and (< pos len)
                  (string-match regex str pos))
        (setq pos (1+ (match-beginning group)))
        (push (match-string group str) matches))
      (nreverse matches))))

(defun nvp-s-all-match-positions (regex str &optional group)
  "Find positions of all REGEX matches in STR for regex GROUP (default 0)."
  (declare (side-effect-free t))
  (or group (setq group 0))
  (save-match-data
    (let ((pos 0) (len (length str)) positions)
      (while (and (< pos len) (string-match regex str pos))
        (setq pos (match-end group))
        (push (cons (match-beginning group) (match-end group)) positions))
      (nreverse positions))))

(provide 'nvp-string)
;;; nvp-string.el ends here
