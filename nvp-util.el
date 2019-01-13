;;; nvp-util ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created:  2 November 2016

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

;; -------------------------------------------------------------------
;;; Date / Time

;; insert date
(defun nvp-insert-date ()
  (interactive)
  (insert (format-time-string "%e %B %Y")))

;; -------------------------------------------------------------------
;;; Lists

;; Intersection of multiple lists.
(defun nvp-list-intersection (l)
  (cond ((null l) nil)
	((null (cdr l)) (car l))
	(t (cl-intersection (car l) (nvp-list-intersection (cdr l))))))

;; Split list LST into N sublists.
(defun nvp-list-split (lst n)
  (cl-loop for i from 0 to (1- (length lst)) by n
     collect (butlast (nthcdr i lst) (- (length lst) (+ n i)))))

;; -------------------------------------------------------------------
;;; Save data

;; recentf.el
(defun nvp-save-variable (var file &optional header coding)
  "Save the `VAR' into `FILE' with `HEADER' and `CODING'."
  (condition-case error
      (with-temp-buffer
        (erase-buffer)
        (set-buffer-file-coding-system (or coding 'emacs-mule))
        (insert (or header
                    (format-message
                     (eval-when-compile
                       (concat
                        ";; -*- mode: emacs-lisp -*-\n"
                        ";;; Automatically generated on %s.\n"))
                     (current-time-string))))
        (nvp-dump-variable var)
        (insert (concat
                 "\n;; Local " "Variables:\n"
                 ";; no-update-autoloads: t\n"
                 ";; coding: " (or coding 'emacs-mule) "\n"
                 ";; End:\n"
                 ";;; " (file-name-nondirectory file)
                 " ends here\n"))
        (write-file (expand-file-name file))
        (message "Saved %s to %s." var file)
        nil)
    (error
     (warn "%s: %s" var (error-message-string error)))))

(defun nvp-dump-variable (variable)
  "Insert a \"(setq VARIABLE value)\" in the current buffer."
  (let ((value (symbol-value variable)))
    (if (atom value)
        (insert (format "\n(setq %S '%S)\n" variable value))
      (insert (format "\n(setq %S\n      '(" variable))
      (dolist (e value)
        (insert (format "\n        %S" e)))
      (insert "\n        ))\n"))))

;; -------------------------------------------------------------------
;;; Procs

(defun nvp-start-process (cmd)
  (start-process
   cmd nil shell-file-name
   shell-command-switch
   (format "nohup 1>/dev/null 2>/dev/null %s" cmd)))

;; -------------------------------------------------------------------
;;; Tables

;; Format alist DAT as an org table.  This alist assumes :head and
;; :rows lists. It splits :rows into number of sublists matching
;; number of colums (:head).
(defun nvp-table-alist-to-org (dat)
  (let* ((head (cdr (assoc-string "head" dat)))
         (rows (append (cdr (assoc-string "rows" dat)) ()))
         (cols (length head)))
    (append head (cons 'hline nil) (nvp-list-split rows cols) nil)))

;; -------------------------------------------------------------------
;;; Strings / Regexp

;; Remove string suffix if it is at the end of the string `s'.
(defsubst nvp-chop-suffix (suffix s)
  (let ((pos (- (length suffix))))
    (if (and (>= (length s) (length suffix))
             (string= suffix (substring s pos)))
        (substring s 0 pos)
      s)))

;; Remove string prefix if is is at the start of `s'.
(defsubst nvp-chop-prefix (prefix s)
  (let ((pos (length prefix)))
    (if (and (>= (length s) (length prefix))
             (string= prefix (substring s 0 pos)))
        (substring s pos)
      s)))

;; Remove trailing whitespace from `STR'.
(defsubst nvp-string-rtrim (str)
  (replace-regexp-in-string "[ \t\n]+$" "" str))

;; Find all matches for `REGEX' within `STR', returning the full match
;; string or group `GROUP'.
(defun nvp-string-all-matches (regex str &optional group)
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))

;; Match `REGEXP' positions in `STR'.
(defun nvp-string-match-positions (regexp str)
  (let ((res '()) (pos 0))
    (while (and (string-match regexp str pos)
		(< pos (length str) ) )
      (let ((m (match-end 0)))
	(push m res)
	(setq pos m)))
    (nreverse res)))

;; make indentation based regexp
(defsubst nvp-re-indent-re ()
  (concat "^\\(?:[ \t]*$\\|"
          (buffer-substring
           (point)
           (save-excursion
             (progn (back-to-indentation) (point))))
          "\\)"))

(defsubst nvp-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

;; Skip back across `backwards' chars, then look for `forward',
;; returning cons of start and end of match.
(defsubst nvp-back-chars-then-look (backwards &optional forward)
  (let ((forward (or forward (format "[%s]+" backwards))))
    (save-excursion
      (skip-chars-backward backwards)
      (if (looking-at forward)
          (cons (point) (match-end 0))
        nil))))

;; -------------------------------------------------------------------
;;; Bindings

;; Overrides a minor mode keybinding for the local buffer by creating
;; or altering keymaps stored in buffer-local variable 
;; `minor-mode-overriding-map-alist'.
(defun nvp-local-minor-mode-key (mode key def)
  (let ((map (make-sparse-keymap)))
    (define-key map key def)
    (push (cons mode map) minor-mode-overriding-map-alist)))

(provide 'nvp-util)
;;; nvp-util.el ends here
