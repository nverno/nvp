;;; nvp-basic-auto.el --- basic autoloads -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-21 13:36:50>
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

;; jump to next char on this line. if matching char,
;; pressing the same key again jumps to the next one, etc.
;;;###autoload
(defun nvp-move-char-this-line (&optional char)
  (interactive (list (char-to-string (read-char "Char: " t))))
  (let ((case-fold-search t))
    (condition-case nil
        (search-forward char (point-at-eol))
      (error (let ((pt (point)))
               (beginning-of-line)
               (or (search-forward char (point-at-eol))
                   (goto-char pt))))))
  (nvp-bind-transient-key
   char (lambda () (interactive) (nvp-move-char-this-line char)) t))

;; used recursively below so not a macro
;;;###autoload
(defun nvp-bind-transient-key (key cmd &optional keep exit)
  "Bind KEY to CMD in transient map."
  (set-transient-map
   (let ((tmap (make-sparse-keymap)))
     (define-key tmap (kbd key) cmd)
     tmap)
   (or keep t)
   (or exit nil)))

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
;;; Assorted
(nvp-declare "" nvp-indicate-pulse-region-or-line)

;;;###autoload
(defun nvp-mark-defun (&optional arg)
  "Mark defun, skipping preceding comments."
  (interactive "p")
  (let ((skip-comments (not (region-active-p))))
    (setq prefix-arg current-prefix-arg)
    (funcall nvp-mark-defun-function arg)
    (and skip-comments (comment-forward (point-max)))))

;;;###autoload
(defun nvp-align (&optional arg beg end)
  "Align buffer region b/w BEG and END, or call `nvp-mark-defun' if nil.
With a single prefix, align entire active region or buffer.
With double prefix, highlight changes that would occur."
  (interactive
   (cons (car current-prefix-arg)
         (if (region-active-p)
             (list (region-beginning) (region-end)))))
  (if (eq 4 arg)                        ;align entire region / buffer
      (if (and beg end) (align beg end)
        (align (point-min) (point-max)))
    (save-mark-and-excursion
      (unless (and beg end)
        (nvp-mark-defun)
        (setq beg (region-beginning) end (region-end)))
        (nvp-indicate-pulse-region-or-line beg end)
      (if (eq 16 arg)                     ;test alignment rule
          (call-interactively 'align-highlight-rule)
        (align beg end)))))

;;;###autoload
(defun nvp-kill-emacs ()
  (interactive)
  (save-some-buffers 'no-ask)
  (kill-emacs))

(provide 'nvp-basic-auto)
;;; nvp-basic-auto.el ends here
