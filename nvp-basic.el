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
(autoload 'string-trim "subr-x")

;; -------------------------------------------------------------------
;;; Movement 

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

(defun nvp-basic-next-defun ()
  (interactive)
  (beginning-of-defun -1))

(defvar-local nvp-basic-header-re nil)
(defun nvp-basic-header-re ()
  (or nvp-basic-header-re
      (setq nvp-basic-header-re
            (let* ((comment (string-trim comment-start))
                   (cs (regexp-quote comment))
                   (multi (> (string-width comment) 1)))
              (if (not multi)
                  ;; ignore things like ';;;###autoload'
                  (format "^\\s-*%s%s\\(?:—\\|---\\|\*\\| |\\|%s[ \t]\\)"
                          cs cs cs)
                (format "^\\s-*%s\\(?:—\\|---\\|%s\\)" cs
                        (regexp-quote (substring comment 1 2))))))))

(defun nvp-basic-next-heading ()
  (interactive)
  (condition-case nil
      (progn
        (forward-line 1)
        (re-search-forward (nvp-basic-header-re))
        (beginning-of-line))
    (error
     (forward-line -1)
     (user-error "No more headings"))))

(defun nvp-basic-previous-heading ()
  (interactive)
  (condition-case nil
      (progn
        (forward-line -1)
        (re-search-backward (nvp-basic-header-re))
        (beginning-of-line))
    (error
     (forward-line 1)
     (user-error "No previous headings"))))

;; -------------------------------------------------------------------
;;; Scrolling 

(autoload 'do-smooth-scroll "smooth-scrolling")
(autoload 'enable-smooth-scroll-for-function "smooth-scrolling")

(when (featurep 'smooth-scrolling)
  (with-no-warnings
    (enable-smooth-scroll-for-function nvp-basic-next5)
    (enable-smooth-scroll-for-function nvp-basic-prev5)
    (enable-smooth-scroll-for-function nvp-basic-next-defun)
    (enable-smooth-scroll-for-function nvp-basic-down-paragraph)
    (enable-smooth-scroll-for-function nvp-basic-up-paragraph)))

;; -------------------------------------------------------------------
;;; Duplicate lines 

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

;; duplicate the current line num times.
(defun nvp-basic-duplicate-current-line (&optional num)
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

(defun nvp-basic--back-and-dupe ()
  (interactive)
  (forward-line -1)
  (nvp-basic-duplicate-current-line))

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
    (while (and (save-excursion
                  (beginning-of-line)
                  (looking-at-p "[[:space:]]*$"))
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

;; -------------------------------------------------------------------
;;; Newline 

(nvp-newline nvp-basic-newline-dwim nil
  :pairs (("{" "}") ("(" ")") ("\\[" "\\]")))

;; -------------------------------------------------------------------
;;; Key bindings 

(defun nvp-basic-temp-binding (key cmd &optional keep exit)
  (set-transient-map
   (let ((tmap (make-sparse-keymap)))
     (define-key tmap (kbd key) cmd)
     tmap)
   (or keep t)
   (or exit nil)))

(provide 'nvp-basic)
;;; nvp-basic.el ends here
