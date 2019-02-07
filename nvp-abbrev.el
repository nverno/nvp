;;; nvp-abbrev.el --- abbrev commands -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-07 11:53:05>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 24 November 2016

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
  (require 'nvp-macro)
  (nvp-local-vars))
(require 'abbrev)
(require 'nvp)
(require 'nvp-abbrev-util)
(declare-function yas-expand-snippet "yasnippet")

(defun nvp-abbrev--read-table (prompt choices)
  (nvp-completing-read prompt choices nil t nil 'minibuffer-history))

;; -------------------------------------------------------------------
;;; Jumping to abbrevs

(defun nvp-abbrev--grab-prev (nchars)
  "Grab preceding NCHARS to match against in abbrev table."
  (save-excursion
    (let ((end (point))
          (_ (skip-chars-backward nvp-abbrev-prefix-chars (- (point) nchars)))
          (start (point)))
      (buffer-substring-no-properties start end))))

;; insert starter abbrev table template
(defun nvp-abbrev--insert-template (table &optional parents)
  (unless parents
    (setq parents (if (derived-mode-p 'prog-mode) '(prog-mode)
                    '(fundamental-mode))))
  (let ((parents
         (concat "(list "
                 (mapconcat (lambda (s) (concat (symbol-name s) "-abbrev-table"))
                            parents " ") ")")))
   (insert
    (concat
     (when (zerop (buffer-size))
       ";; -*- coding: utf-8; mode: emacs-lisp; no-byte-compile: t; -*-\n")
     "\n(define-abbrev-table '" table "\n"
     "  '()\n"
     (format
      "  \"%s Abbrevs.\"\n"
      (capitalize
       (replace-regexp-in-string "-abbrev-table" "" table)))
     (format "  :parents %s)" parents)))))

;; open abbrev file and search for the specified table
;; if it doesn't exist insert starter template
(defun nvp-abbrev--get-table (table file)
  (find-file-other-window file)
  (goto-char (point-min))
  (unless (search-forward-regexp (concat "'" table "\\>") nil t)
    (goto-char (point-max))
    (nvp-abbrev--insert-template table)))

;; reload abbrevs after modification
(defun nvp-abbrev-after-save-hook ()
  (and (buffer-modified-p (current-buffer))
       (quietly-read-abbrev-file buffer-file-name)))

;; -------------------------------------------------------------------
;;; Commands

;;;###autoload
(defun nvp-abbrev-jump-to-file (arg)
  "Jump to abbrev file, `nvp-abbrev-local-table', and search for insertion location.
Prefix ARG specifies the length of the preceding text to use as abbrev.
When abbrev text is selected, searching is done first by length then lexically."
  (interactive "P")
  (let* ((file-abbr (bound-and-true-p nvp-abbrev-local-table))
         (table (regexp-quote (format "%s-abbrev-table" (or file-abbr major-mode))))
         (prefix (if arg (nvp-abbrev--grab-prev arg))))
    (if (bound-and-true-p nvp-abbrev-local-file)
        (nvp-abbrev--get-table table nvp-abbrev-local-file)
      (nvp-abbrev--get-table table (expand-file-name table nvp/abbrevs)))
    (goto-char (point-min))
    (search-forward-regexp (concat "'" table "\\>") nil t)
    (when prefix
      (while
          (and (re-search-forward "[^\\(?:(define-\\)](\"\\(\\w+\\)" nil 'move)
               (let ((str (match-string-no-properties 1)))
                 (or (> (length prefix) (length str))
                     (string> prefix str)))))
      ;; insert default template for prefix
      (back-to-indentation)
      (yas-expand-snippet
       (format "(\"%s\" \"$1\" nil :system t)\n" prefix))))
    ;; reload abbrev table after modification
    (add-hook 'after-save-hook #'nvp-abbrev-after-save-hook t 'local))

;; add unicode abbrevs to local table parents
;;;###autoload
(defun nvp-abbrev-add-parent (table &optional file)
  "Add abbrev TABLE as parent of `local-abbrev-table'.
If FILE is non-nil, read abbrevs from FILE."
  (interactive
   (list (nvp-abbrev--read-table
          "Add parent abbrev table: "
          (mapcar #'symbol-name abbrev-table-name-list))))
  (when file
    (quietly-read-abbrev-file file))
  (let ((parents (abbrev-table-get local-abbrev-table :parents)))
    (abbrev-table-put
     local-abbrev-table :parents (cons (symbol-value (intern table)) parents)))
  (message "Activated %s abbrevs locally." table))

;;;###autoload
(defun nvp-abbrev-load-unicode ()
  "Add unicode abbrevs as parent of local abbrev table."
  (interactive)
  (nvp-abbrev-add-parent
   "unicode-latex-abbrev-table"
   (expand-file-name "unicode-latex-abbrev-table" (nvp-package-root))))

;;;###autoload
(defun nvp-abbrev-remove-parent (table &optional parents)
  "Remove parent TABLE from `local-abbrev-table' PARENTS."
  (interactive
   (let ((parents
          (mapcar #'abbrev-table-name
                  (abbrev-table-get local-abbrev-table :parents))))
     (list (intern (nvp-abbrev--read-table
                    "Remove local abbrev parent: " (mapcar #'symbol-name parents)))
           parents)))
  (let ((new-p (mapcar #'symbol-value (remq table parents))))
    (abbrev-table-put local-abbrev-table :parents new-p))
  (message "Removed local %S abbrevs." table))

;; write abbrev table
;; temporarily rebind `abbrev--write' to write :system abbrevs
;;;###autoload
(defun nvp-abbrev-write-abbrev-table (table file)
  "Write abbrev TABLE to FILE as :system abbrevs."
  (interactive
   (list
    (if current-prefix-arg
        (abbrev-table-name local-abbrev-table)
      (nvp-abbrev--read-table
       "Abbrev table: " (mapcar #'symbol-name (nvp-abbrev--nonempty))))
    (read-file-name "Write abbrevs to: ")))
  (let ((abbrev-table-name-list (list (intern table))))
    (cl-letf (((symbol-function 'abbrev--write)
               (lambda (sym)
                 (unless (null (symbol-value sym))
                   (insert "    (")
                   (prin1 (symbol-name sym))
                   (insert " ")
                   (prin1 (symbol-value sym))
                   (insert " ")
                   (prin1 (symbol-function sym))
                   (insert " :system t)\n")))))
      (write-abbrev-file file))))

;;;###autoload
(defun nvp-abbrev-properties (table)
  "List all abbrev table properties."
  (interactive
   (list
    (if current-prefix-arg
        (abbrev-table-name local-abbrev-table)
      (nvp-abbrev--read-table
       "List abbrev table props: "
       (mapcar #'symbol-name (nvp-abbrev--nonempty))))))
  (let ((props (nvp-abbrev--get-plist (intern table))))
    (nvp-with-results-buffer nil
      (princ (format "%S\n--------------------\n" table))
      (pcase-dolist (`(,k ,v) props)
        (princ (format "%S: %S\n" k v))))))

;; unlike default, list all parent tables and dynamic tables as well
;;;###autoload
(defun nvp-abbrev-list-abbrevs (&optional all)
  "List all local abbrev tables by default.
If ALL is non-nill, list all abbrev tables."
  (interactive "P")
  (if all
      (display-buffer (prepare-abbrev-list-buffer))
    (let ((abbrev-table-name-list
           (cons (abbrev-table-name local-abbrev-table)
                 (nvp-abbrev--all-parents local-abbrev-table 'names))))
      (and nvp-abbrev-dynamic-table
           (push 'nvp-abbrev-dynamic-table abbrev-table-name-list))
      (display-buffer (prepare-abbrev-list-buffer nil)))))

(provide 'nvp-abbrev)
;;; nvp-abbrev.el ends here
