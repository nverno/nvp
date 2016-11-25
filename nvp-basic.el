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
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))

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


;;--- Duplicate Line -------------------------------------------------

(declare-function paredit-kill "paredit")

;; Duplicates the current line or region arg times.
;; if there's no region, the current line will be duplicated
;; (or last non-empty).
(defun nvp-basic-duplicate-line-or-region (arg)
  (interactive "p")
  (if (region-active-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (nvp-basic--duplicate-region arg beg end))
  (nvp-basic--duplicate-last-nonempty-line arg)
  (nvp-basic-temp-binding "d" #'nvp-basic--back-and-dupe)))

(defun nvp-basic--back-and-dupe ()
  (interactive)
  (forward-line -1)
  (nvp-basic--duplicate-current-line))

;; duplicates the region bounded by start and end num times.
;; if no start and end is provided, the current region-beginning and
;; region-end is used.
(defun nvp-basic--duplicate-region (&optional num start end)
  (interactive "p")
  (save-excursion
    (let* ((start (or start (region-beginning)))
	   (end (or end (region-end)))
	   (region (buffer-substring start end)))
      (goto-char end)
      (dotimes (_i num)
	(insert region)))))

;; Duplicate the current of previous line that isn't blank.
(defun nvp-basic--duplicate-last-nonempty-line (&optional num)
  (interactive "p")
  (let ((back 0))
    (while (and (nvp--line-empty-p)
                (> (line-number-at-pos) 1))
      (forward-line -1)
      (setq back (1+ back)))
    (when (eq (point-at-eol) (point-max))
      (goto-char (point-max))
      (newline)
      (forward-char -1))
    (let ((region (buffer-substring (point-at-bol)
                                    (1+ (point-at-eol)))))
      (forward-line back)
      (dotimes (_i num)
        (insert region))))
  (goto-char (point-at-eol)))

(defun nvp-basic--paredit-duplicate-current-line ()
  (back-to-indentation)
  (let (kill-ring kill-ring-yank-pointer)
    (paredit-kill)
    (yank)
    (newline-and-indent)
    (yank)))

;; duplicate the current line num times.
(defun nvp-basic--duplicate-current-line (&optional num)
  (interactive "p")
  (if (bound-and-true-p paredit-mode)
      (nvp-basic--paredit-duplicate-current-line)
    (save-excursion
      (when (eq (point-at-eol) (point-max))
        (goto-char (point-max))
        (newline)
        (forward-char -1))
      (nvp-basic--duplicate-region num (point-at-bol)
                             (1+ (point-at-eol))))))

;;--- Newline --------------------------------------------------------

(nvp-newline nvp-basic-newline-dwim nil
  :pairs (("{" "}") ("(" ")") ("\\[" "\\]")))

;;--- Keys -----------------------------------------------------------

(defun nvp-basic-temp-binding (key cmd &optional keep exit)
  (set-transient-map
   (let ((tmap (make-sparse-keymap)))
     (define-key tmap (kbd key) cmd)
     tmap)
   (or keep t)
   (or exit nil)))

(provide 'nvp-basic)
;;; nvp-basic.el ends here
