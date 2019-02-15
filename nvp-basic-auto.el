;;; nvp-basic-auto.el --- basic autoloads -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-14 15:46:41>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Maintainer: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires:
;; Created:  2 February 2019

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
  (require 'subr-x)
  (require 'nvp-macro)
  (require 'hydra))
(require 'nvp)
(declare-function helm-show-kill-ring "")

;; -------------------------------------------------------------------
;;; Movement
;; see `paragraph-start' and `paragraph-separate' to extend

;;;###autoload
(defun nvp-move-forward-paragraph (&optional arg)
  (interactive "^p")
  (or arg (setq arg 1))
  (if (bolp)
      (progn
        (and (< arg 1) (forward-line -1))
        (forward-paragraph arg)
        (forward-line 1))
    (line-move arg)))

;;;###autoload
(defun nvp-move-backward-paragraph (&optional arg)
  (interactive "^p")
  (or arg (setq arg 1))
  (nvp-move-forward-paragraph (- arg)))

;; -------------------------------------------------------------------
;;; Yank / Pop

;;;###autoload(autoload 'nvp-hydra-yank-pop/yank-pop "nvp-basic-auto")
;;;###autoload(autoload 'nvp-hydra-yank-pop/yank "nvp-basic-auto")
(defhydra nvp-hydra-yank-pop ()
  "yank"
  ("C-y" yank nil)
  ("M-y" yank-pop nil)
  ("y" (yank-pop 1) "next")
  ("Y" (yank-pop -1) "prev")
  ("l" helm-show-kill-ring "list" :color blue))

;; -------------------------------------------------------------------
;;; Paredit
(nvp-declare "paredit" paredit-delete-indentation)

;;;###autoload
(defun nvp-paredit-remove-newlines ()
  "Removes extra whitespace and newlines from current point to the next paren."
  (interactive)
  (let ((up-to (point)))
    (backward-char)
    (while (> (point) up-to)
      (nvp-paredit-delete-indentation))))

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

;; -------------------------------------------------------------------
;;; Duplicate lines 

(declare-function paredit-kill "paredit")

;; Duplicates the current line or region arg times.
;; if there's no region, the current line will be duplicated
;; (or last non-empty).
;;;###autoload
(defun nvp-duplicate-line-or-region (arg)
  (interactive "p")
  (if (region-active-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (nvp--duplicate-region arg beg end))
    (nvp--duplicate-last-nonempty-line arg)
    (nvp-bind-transient-key "d" #'nvp--duplicate-back-and-dupe)))

;; duplicate the current line num times.
(defun nvp-duplicate-current-line (&optional num)
  (interactive "p")
  (if (bound-and-true-p paredit-mode)
      (nvp--paredit-duplicate-current-line)
    (save-excursion
      (when (eq (point-at-eol) (point-max))
        (goto-char (point-max))
        (newline)
        (forward-char -1))
      (nvp--duplicate-region num (point-at-bol) (1+ (point-at-eol))))))

(defun nvp--duplicate-back-and-dupe ()
  (interactive)
  (forward-line -1)
  (nvp-duplicate-current-line))

;; duplicates the region bounded by start and end num times.
;; if no start and end is provided, the current region-beginning and
;; region-end is used.
(defun nvp--duplicate-region (&optional num start end)
  (interactive "p")
  (save-excursion
    (let* ((start (or start (region-beginning)))
	   (end (or end (region-end)))
	   (region (buffer-substring start end)))
      (goto-char end)
      (dotimes (_i num)
	(insert region)))))

;; Duplicate the current of previous line that isn't blank.
(defun nvp--duplicate-last-nonempty-line (&optional num)
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

(defun nvp--paredit-duplicate-current-line ()
  (back-to-indentation)
  (let (kill-ring kill-ring-yank-pointer)
    (paredit-kill)
    (yank)
    (newline-and-indent)
    (yank)))

;; -------------------------------------------------------------------
;;; Assorted

;;;###autoload
(defun nvp-kill-emacs ()
  (interactive)
  (save-some-buffers 'no-ask)
  (kill-emacs))

(provide 'nvp-basic-auto)
;;; nvp-basic-auto.el ends here
