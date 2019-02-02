;;; nvp-abbrev.el --- abbrev commands -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-02 01:35:16>
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
(require 'expand)
(require 'abbrev)
(require 'nvp)

;; abbrev table read in mode hooks
(defvar-local nvp-abbrev-local-table nil)

;; abbrev prefixes
(defvar nvp-abbrev-prefix-chars "A-Za-z0-9#.")

;;;###autoload
(defun nvp-abbrev-jump-to-file (arg)
  "Jump to abbrev file, `nvp-abbrev-local-table', and search for insertion location.
Prefix ARG specifies the length of the preceding text to use as abbrev.
When abbrev text is selected, searching is done first by length then lexically."
  (interactive "P")
  (let* ((file-abbr (bound-and-true-p nvp-abbrev-local-table))
         (table (regexp-quote (format "%s-abbrev-table"
                                      (or file-abbr major-mode))))
         (pref (if arg (nvp-abbrev--grab-prev arg))))
    (if (bound-and-true-p nvp-abbrev-local-file)
        (nvp-abbrev--get-table table nvp-abbrev-local-file)
      (nvp-abbrev--get-table
       table (expand-file-name table nvp/abbrevs)))
    (goto-char (point-min))
    (search-forward-regexp (concat "'" table "\\>") nil t)
    (when pref
      (while
          (and
           (re-search-forward "[^\\(?:(define-\\)](\"\\(\\w+\\)"
                              nil t)
           (let ((str (buffer-substring-no-properties
                       (match-beginning 1) (match-end 1))))
             (if (> (length pref) (length str))
                 t
               (string> pref str)))))
      ;; insert default template for prefix
      (back-to-indentation)
      (insert (format "(\"%s\" \"\" nil :system t)\n" pref))
      (indent-according-to-mode)
      (backward-char (+ (current-indentation) 17)))
    ;; reload abbrev table after modification
    (add-hook 'after-save-hook
              #'(lambda () (quietly-read-abbrev-file buffer-file-name))
              t 'local)))

;; add unicode abbrevs to local table parents
;;;###autoload
(defun nvp-abbrev-add-parent (table &optional file)
  "Add abbrev TABLE as parent of `local-abbrev-table'.
If FILE is non-nil, read abbrevs from FILE."
  (interactive
   (list (nvp-completing-read
          "Add parent abbrev table: "
          (mapcar #'symbol-name abbrev-table-name-list))))
  (when file
    (quietly-read-abbrev-file file))
  (let ((parents (abbrev-table-get local-abbrev-table :parents)))
    (abbrev-table-put
     local-abbrev-table
     :parents (cons (symbol-value (intern table)) parents)))
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
     (list (intern (nvp-completing-read
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
      (nvp-completing-read
       "Abbrev table: " (mapcar 'symbol-name
                                (nvp-abbrev--abbrev-list))))
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

;; -------------------------------------------------------------------
;;; Expand Hooks

;; allow abbrevs to expand inside parens
;;;###autoload
(defun nvp-abbrev-expand-in-paren-hook ()
  (cl-letf (((symbol-function 'eolp)
             #'(lambda () (not (eq (char-syntax (char-after)) ?w)))))
    (expand-abbrev-hook)))

;; -------------------------------------------------------------------
;;; Expansion predicates

;; dont expand in strings/comments
;;;###autoload
(defun nvp-abbrev-expand-p ()
  (let ((ppss (syntax-ppss)))
    (not (or (elt ppss 3) (elt ppss 4)))))

;; don't expand in strings/comments or after [_.-:]
;;;###autoload
(defun nvp-abbrev-expand-not-after-punct-p ()
  (and (not (memq last-input-event '(?_ ?. ?- ?:)))
       (let ((ppss (syntax-ppss)))
         (not (or (elt ppss 3) (elt ppss 4))))))

;; insert starter abbrev table template
(defun nvp-abbrev--insert-template (table)
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
    "  :parents (list prog-mode-abbrev-table))")))

;; open abbrev file and search for the specified table
;; if it doesn't exist insert starter template
(defun nvp-abbrev--get-table (table file)
  (find-file-other-window file)
  (goto-char (point-min))
  (unless (search-forward-regexp (concat "'" table "\\>") nil t)
    (goto-char (point-max))
    (nvp-abbrev--insert-template table)))

;; grab preceding characters to match agains in abbrev table
(defsubst nvp-abbrev--grab-prev (arg)
  (save-excursion
    (let ((end (point))
          (_ (skip-chars-backward nvp-abbrev-prefix-chars (- (point) arg)))
          (start (point)))
      (buffer-substring-no-properties start end))))

;; return list of currently loaded and non-empty abbrev tables
(defsubst nvp-abbrev--abbrev-list (&optional tables)
  (cl-remove-if
   #'(lambda (table)
       (abbrev-table-empty-p (symbol-value table)))
   (or tables abbrev-table-name-list)))

;; list of active and non-empty abbrev tables
(defsubst nvp-abbrev--active-tables ()
  (nvp-abbrev--abbrev-list (mapcar #'abbrev-table-name (abbrev--active-tables))))

;; -------------------------------------------------------------------
;;; Completion

;; `company-abbrev' doesn't account for :regexp properties, so doesnt
;; work properly when abbrev tables define their own :regexp,
;; ie "\\degree" or "#inc"

(declare-function company-grab-symbol "company")

;; Return completion candidates
;; take into account per-table :regexp. If not defined, default to
;; `company-grab-symbol'
(defun nvp-abbrev--complete-candidates ()
  (interactive)
  (let ((tables (nvp-abbrev--active-tables))
        prefix re comps)
    (dolist (table tables)
      (setq re (abbrev-table-get (symbol-value table) :regexp))
      (setq prefix
            (if re
                (and (looking-back re (line-beginning-position))
                     (match-string 1))
              (company-grab-symbol)))
      (and prefix
           (setq comps
                 (nconc
                  (delete "" (all-completions prefix (symbol-value table)))
                  comps))))
    comps))

(provide 'nvp-abbrev)
;;; nvp-abbrev.el ends here
