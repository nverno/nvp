;;; nvp-basic.el --- basic requires -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-02-21 13:14:57>
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
;; Stuff required during init
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib)
  (require 'subr-x))
(require 'nvp)
(declare-function company-quickhelp-manual-begin "company-quickhelp")
(declare-function do-smooth-scroll "smooth-scrolling")
(declare-function minibuffer-keyboard-quit "delsel")
(nvp-declare "paredit"
  paredit-close-round paredit-find-comment-on-line paredit-move-past-close)

;; -------------------------------------------------------------------
;;; Movement

(defun nvp-move-next5 (&rest _ignored)
  (interactive)
  (forward-line 5))

(defun nvp-move-prev5 (&rest _ignored)
  (interactive)
  (forward-line -5))

;; add smooth-scrolling
(nvp-advise-commands 'do-smooth-scroll :after (nvp-move-next5 nvp-move-prev5))

(defun nvp-move-forward-defun (&rest _ignored)
  (interactive)
  (beginning-of-defun -1))

(defalias 'nvp-move-backward-defun 'beginning-of-defun)

;;; Headings
;; these may vary by mode
(defun nvp-move-header-regex ()
  "Get or create header regex based on comment syntax."
  (or nvp-move-header-regex
      (setq nvp-move-header-regex
            (let* ((comment (string-trim comment-start))
                   (cs (regexp-quote comment))
                   (multi (> (string-width comment) 1)))
              (if (not multi)
                  ;; ignore things like ';;;###autoload'
                  (format "^\\s-*%s%s\\(?:—\\|---\\|\*\\| |\\|%s\\)\\s-"
                          cs cs cs)
                (format "^\\s-*%s\\(?:—\\|---\\|%s\\)\\s-" cs
                        (regexp-quote (substring comment 1 2))))))))

(defun nvp-move-forward-heading (&optional back)
  (interactive)
  (condition-case nil
      (progn
        (forward-line (if back -1 1))
        (if back (re-search-backward (nvp-move-header-regex))
          (re-search-forward (nvp-move-header-regex)))
        (beginning-of-line))
    (error
     (forward-line (if back 1 -1))
     (user-error (format "No %s headings" (if back "previous" "more"))))))

(defun nvp-move-previous-heading (&rest _ignored)
  (interactive)
  (nvp-move-forward-heading 'back))

;;; Newlines
;; generics with defaults - lisp modes don't do anything special
;;;###autoload
(cl-defgeneric nvp-newline-dwim-prefix (&optional arg)
  "Generic function to handle newline dwim in special contexts."
  (newline arg 'interactive))

;;;###autoload
(cl-defgeneric nvp-newline-dwim-syntax (&optional arg _syntax _comment-cont)
  "Generic function to handle newline dwim syntactically."
  (newline arg 'interactive))

;;;###autoload
(cl-defgeneric nvp-newline-dwim-default (&optional arg _pairs)
  "Generic function to handle default newline dwim."
  (newline arg 'interactive))

(defun nvp-newline-dwim (&optional arg)
  "Newline dwim.
Dispatches to generic handlers with ARG."
  (interactive "*P")
  (funcall-interactively 'nvp-newline-dwim-default arg))

;; -------------------------------------------------------------------
;;; Paredit

(defun nvp-paredit-close-round (&optional arg)
  "Close paren skipping over possible comments and call `expand-abbrev'.
With ARG use default behaviour, except also call `expand-abbrev'."
  (interactive "P")
  (expand-abbrev)
  (if arg (paredit-close-round)
    (let ((beg (point)) ;keep comment on same line
          (cmt (paredit-find-comment-on-line)))
      (paredit-move-past-close ?\))
      (and cmt (save-excursion
                 (unless (eq (line-number-at-pos) (line-number-at-pos beg))
                   (goto-char beg))
                 (insert (car cmt)))))))

;; -------------------------------------------------------------------
;;; Company

(defun nvp-company-quickhelp-toggle ()
  "Toggle pos-tip help on/off."
  (interactive)
  (let ((x-gtk-use-system-tooltips nil))
    (or (x-hide-tip)
        ;;   (add-hook 'pre-command-hook #'company-pre-command nil t)
        ;; (remove-hook 'pre-command-hook #'company-pre-command t)
        ;; (cl-letf (((symbol-function 'company--electric-restore-window-configuration)
        ;;            #'ignore)))
        (company-quickhelp-manual-begin))))

;;; FIXME: check if backend is already there -- this could be a macro
(defun nvp-company-local (backend)
  "Make a buffer-local company backend."
  (set (make-local-variable 'company-backends)
       (push backend company-backends)))

;; -------------------------------------------------------------------
;;; IDO

(defun nvp-ido-refresh-homedir ()
  "Refresh completion for homedir while ido is finding a file."
  (interactive)
  (ido-set-current-directory "~/")
  (setq ido-exit 'refresh)
  (exit-minibuffer))

(defun nvp-ido-yank ()
  "Forward to `yank'."
  (interactive)
  (if (file-exists-p (current-kill 0))
      (ido-fallback-command)
    (yank)))

(defun nvp-ido-backspace ()
  "Forward to `backward-delete-char'.
On error (read-only), quit without selecting."
  (interactive)
  (condition-case nil
      (backward-delete-char 1)
    (error
     (minibuffer-keyboard-quit))))

(defun nvp-ido-beginning-of-input ()
  (interactive)
  (goto-char (minibuffer-prompt-end)))

;; -------------------------------------------------------------------
;;; Wrapper functions

(nvp-wrapper-fuctions
 (nvp-check-buffer-function . nil)
 (nvp-repl-switch-function  . nil)
 (nvp-test-function         . nil)
 (nvp-tag-function          . nil))

(provide 'nvp-basic)
;;; nvp-basic.el ends here
