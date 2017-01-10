;;; nvp-expand ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 10 January 2017

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
  (require 'cl-lib))

;;;###autoload
(defun nvp-expand-range ()
  "Expand range before point, eg. to make 0:10 by 1 could be 
either m:10, m0:10, m0:1:10 or m:1:10. So, a missing start assumes starts from 0."
  (interactive)
  (when (looking-back (nvp-concat "\\bm\\(-?[0-9.]*\\)\:"
                                  ;; either end of range or increment
                                  "\\(-?[0-9.]+\\)"
                                  ;; optional end of range
                                  "\\(?::\\(-?[0-9.]+\\)\\)?"))
    (let ((start (match-string 1))
          (inc (match-string 2))
          (end (match-string 3)))
      (condition-case str
          (let ((res (mapconcat 'number-to-string
                                (number-sequence
                                 (string-to-number start)
                                 (string-to-number (or end inc))
                                 (and end (string-to-number inc)))
                                " ")))
            (delete-region (match-beginning 0) (point))
            (insert res))
        (error)))))

;;;###autoload
(defun nvp-expand-tiny (&optional arg)
  "Expand region before point with `tiny-expand'. If prefix is non-nil,
wrap expanded items with quotes (default) or with double-prefix, prompt
for string to wrap."
  (interactive "P")
  (cond
   ((eq (car-safe arg) 4)
    (progn
      (tiny-expand)
      (nvp-list-wrap-quotes tiny-beg (point))))
   ((eq (car-safe arg) 16)
    (progn
      (tiny-expand)
      (nvp-list-wrap-quotes tiny-beg (point) 'prompt)))
   (t (tiny-expand))))

(provide 'nvp-expand)
;;; nvp-expand.el ends here
