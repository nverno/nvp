;;; nvp-abbrev.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-02-01 21:38:59>
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
  (require 'nvp-macro)
  (require 'cl-lib)
  (defvar nvp/abbrevs))
(require 'expand)
(require 'abbrev)

;; abbrev table read in mode hooks
(defvar-local nvp-abbrev-local-table nil)

;; abbrev prefixes
(defvar nvp-abbrev-prefix-chars "A-Za-z0-9#.")

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
  (nvp-abbrev--abbrev-list
   (mapcar 'abbrev-table-name (abbrev--active-tables))))

;; add unicode abbrevs to local table parents
;;;###autoload
(defun nvp-abbrev-add-parent (table &optional file)
  (interactive
   (list (ido-completing-read
          "Abbrev table: "
          (mapcar 'symbol-name abbrev-table-name-list) nil nil)))
  (when file
    (quietly-read-abbrev-file file))
  (let ((parents (abbrev-table-get local-abbrev-table :parents)))
    (abbrev-table-put
     local-abbrev-table
     :parents (cons (symbol-value (intern table)) parents)))
  (message "loaded %s" table))

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

;; -------------------------------------------------------------------
;;; Expand Hooks

;; allow abbrevs to expand inside parens
;;;###autoload
(defun nvp-abbrev-expand-hook ()
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

(provide 'nvp-abbrev)
;;; nvp-abbrev.el ends here
