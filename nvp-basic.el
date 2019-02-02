;;; nvp-basic.el --- general functions -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-02-02 02:37:25>
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
;; Required in init
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(autoload 'string-trim "subr-x")
(declare-function nvp-bind-transient-key "nvp-bind")

;; -------------------------------------------------------------------
;;; Movement 

;; jump to next char on this line. if matching char,
;; pressing the same key again jumps to the next one, etc.
(defun nvp-basic-char-this-line (&optional char)
  (interactive (list (char-to-string (read-char "Char: " t))))
  (let ((case-fold-search t))
    (condition-case nil
        (search-forward char (point-at-eol))
      (error (let ((pt (point)))
               (beginning-of-line)
               (or (search-forward char (point-at-eol))
                   (goto-char pt))))))
  (nvp-bind-transient-key
   char (lambda () (interactive) (nvp-basic-char-this-line char)) t))

(defun nvp-basic-next5 (&rest _ignored)
  (interactive)
  (forward-line 5))

(defun nvp-basic-prev5 (&rest _ignored)
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

(defun nvp-basic-next-defun (&rest _ignored)
  (interactive)
  (beginning-of-defun -1))

;;; Headings
;; these may vary by mode

(defvar-local nvp-basic-header-re nil)
(defun nvp-basic-header-re ()
  (or nvp-basic-header-re
      (setq nvp-basic-header-re
            (let* ((comment (string-trim comment-start))
                   (cs (regexp-quote comment))
                   (multi (> (string-width comment) 1)))
              (if (not multi)
                  ;; ignore things like ';;;###autoload'
                  (format "^\\s-*%s%s\\(?:—\\|---\\|\*\\| |\\|%s\\)\\s-"
                          cs cs cs)
                (format "^\\s-*%s\\(?:—\\|---\\|%s\\)\\s-" cs
                        (regexp-quote (substring comment 1 2))))))))

(defun nvp-basic-next-heading (&rest _ignored)
  (interactive)
  (condition-case nil
      (progn
        (forward-line 1)
        (re-search-forward (nvp-basic-header-re))
        (beginning-of-line))
    (error
     (forward-line -1)
     (user-error "No more headings"))))

(defun nvp-basic-previous-heading (&rest _ignored)
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
(declare-function do-smooth-scroll "smooth-scrolling")

(nvp-advise-commands
  'do-smooth-scroll :after
  (nvp-basic-next5 nvp-basic-prev5 nvp-basic-next-defun
                   nvp-basic-down-paragraph nvp-basic-up-paragraph))

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
  (nvp-bind-transient-key "d" #'nvp-basic--back-and-dupe)))

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
;;; Paredit
(nvp-declare "paredit"
  paredit-close-round paredit-find-comment-on-line paredit-move-past-close)

(defun nvp-paredit-close-round (&optional arg)
  "Close paren skipping over possible comments.
With ARG use default behaviour."
  (interactive "P")
  (if arg (paredit-close-round)
    (let ((beg (point)) ;keep comment on same line
          (cmt (paredit-find-comment-on-line)))
      (paredit-move-past-close ?\))
      (and cmt (save-excursion
                 (unless (eq (line-number-at-pos) (line-number-at-pos beg))
                   (goto-char beg))
                 (insert (car cmt)))))))

;; https://www.emacswiki.org/emacs/ParEdit
(defun nvp-paredit-delete-indentation (&optional arg)
  "Handle joining lines that end in a comment."
  (interactive "P")
  (let (comt)
    (save-excursion
      (move-beginning-of-line (if arg 1 0))
      (when (skip-syntax-forward "^<" (point-at-eol))
        (setq comt (delete-and-extract-region (point) (point-at-eol))))
      (delete-indentation arg)
      (when comt
        (save-excursion
          (move-end-of-line 1)
          (insert " ")
          (insert comt))))))

(defun nvp-paredit-remove-newlines ()
  "Removes extra whitespace and newlines from current point to the next paren."
  (interactive)
  (let ((up-to (point)))
    (backward-char)
    (while (> (point) up-to)
      (nvp-paredit-delete-indentation))))

;; -------------------------------------------------------------------
;;; Newline 

(nvp-newline nvp-basic-newline-dwim nil
  :pairs (("{" "}") ("(" ")") ("\\[" "\\]")))

(provide 'nvp-basic)
;;; nvp-basic.el ends here
