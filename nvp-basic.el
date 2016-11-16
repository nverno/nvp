;;; nvp-basic ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 16 November 2016

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

;;--- Movement -------------------------------------------------------

;; jump to next char on this line. if matching char,
;; pressing the same key again jumps to the next one, etc.
(defun nvp-basic-char-this-line (&optional char)
  (interactive (list (char-to-string (read-char "Char: " t))))
  (let ((case-fold-search t))
    (search-forward char (point-at-eol) t))
  (nvp-basic-temp-binding
   char (lambda () (interactive) (nvp-basic-char-this-line char)) t))

(defun nvp-basic-next5()
  (interactive)
  (forward-line 5))

(defun nvp-basic-prev5()
  (interactive)
  (forward-line -5))

(defun nvp-basic-down-paragraph (arg)
  (interactive "p")
  (if (bolp)
      (progn
        (forward-paragraph arg)
        (forward-line 1))
    (line-move arg)))

(defun nvp-basic-up-paragraph (arg)
  (interactive "p")
  (if (bolp)
      (progn
        (forward-line -1)
        (backward-paragraph arg)
        (forward-line 1))
    (line-move (- arg))))

;;--- Keys -----------------------------------------------------------

;; make a fleeting keybinding
(defun nvp-basic-temp-binding (key cmd &optional keep exit)
  (set-transient-map
   (let ((tmap (make-sparse-keymap)))
     (define-key tmap (kbd key) cmd)
     tmap)
   (or keep t)
   (or exit nil)))

(provide 'nvp-basic)
;;; nvp-basic.el ends here
