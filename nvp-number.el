;;; nvp-number.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-02-11 02:10:27>
;; Package-Requires: 
;; Created:  1 October 2017

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
  (require 'nvp-macro))
(declare-function hexl-hex-char-to-integer "hexl")
(declare-function hexl-oct-char-to-integer "hexl")

;; -------------------------------------------------------------------
;;; Conversion

;; from hexl.el
;;;###autoload
(defun nvp-number-hex-string-to-integer ()
  (interactive)
  (require 'hexl)
  (message "%d" (call-interactively #'hexl-hex-string-to-integer)))

;;;###autoload
(defun nvp-number-octal-string-to-integer ()
  (interactive)
  (require 'hexl)
  (message "%d" (call-interactively #'hexl-octal-string-to-integer)))

;; -------------------------------------------------------------------
;;; Popups
;; momentary-string-display #<marker at 110240 in subr.el.gz>

;;;###autoload
(defun nvp-number-show-decimal (symbol)
  (interactive (list (thing-at-point 'symbol)))
  (cond
   ((string-match "$?0x" symbol)
    (nvp-with-toggled-tip
     (number-to-string
      (string-to-number (substring symbol (match-end 0)) 16))
     :help-fn nil))))

;; https://github.com/skeeto/.emacs.d/blob/master/lisp/extras.el#L83
;;;###autoload
(defun nvp-number-insert-random (n)
  "Insert random number b/w 0 and prefix."
  (interactive "P")
  (insert (number-to-string (random n))))

;;;###autoload
(cl-defun nvp-number-insert-random-hex (&optional (size 64))
  "Insert a random, SIZE-bit number as hex."
  (interactive)
  (let ((string (make-string (/ size 4) 0))
        (digits "0123456789abcdef"))
    (dotimes (i (/ size 4))
      (setf (aref string i) (aref digits (cl-random 16))))
    (insert string)))

(provide 'nvp-number)
;;; nvp-number.el ends here
