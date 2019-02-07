;;; nvp-abbrev-company.el --- local abbrev completion -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-07 03:20:52>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Maintainer: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created:  6 February 2019

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

;; `company-abbrev' doesn't account for :regexp properties, so doesnt
;; work properly when abbrev tables define their own :regexp,
;; ie "\\degree" or "#inc"

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(require 'company)
(require 'nvp-abbrev)

;; use local table along with its parents + global table
(defvar-local nvp-abbrev-completion--tables nil)
(defun nvp-abbrev-completion--tables ()
  (or nvp-abbrev-completion--tables
      (setq nvp-abbrev-completion--tables
            (cl-remove-duplicates
             (append (list (abbrev-table-name local-abbrev-table))
                     (nvp-abbrev--all-parents local-abbrev-table)
                     '(global-abbrev-table))))))

;; add completion annotations
(defun nvp-abbrev-completion--apply-annotation (table)
  (let ((tab-name (symbol-name table)))
    (mapatoms (lambda (sym) (add-text-properties 0 1 (list 'annotation tab-name) sym)))))

;; active tables at point
(defun nvp-abbrev-completion--active-tables ()
  (cl-remove-if-not (lambda (tab)
                      (let ((pred (abbrev-table-get
                                   (symbol-value tab) :enable-function)))
                        (if pred (funcall pred) t)))
                    (nvp-abbrev-completion--tables)))

;; Return first prefix from tables that satisfies its `:enable-function'
;; and matches its table's `:regexp'
(defun nvp-abbrev-completion--prefix ()
  (cl-loop for tab in (nvp-abbrev-completion--active-tables)
     as re = (abbrev-table-get (symbol-value tab) :regexp)
     when (and re (looking-back re (line-beginning-position)))
     return (match-string-no-properties 1)))

;; return prefix, either matching a table's predicates or defaulting to the
;; previous symbol
(defun nvp-abbrev-completion-prefix ()
  (or (nvp-abbrev-completion--prefix)
      (nvp-grab-previous-symbol)))

;; Return completion candidates, taking into account per-table :regexp
(defun nvp-abbrev-completion-candidates (arg)
  (cl-loop for tab in (nvp-abbrev-completion--active-tables)
     nconc (delete "" (all-completions arg (symbol-value tab)))))

;; -------------------------------------------------------------------
;;; Company

;;;###autoload
(defun nvp-company-abbrev (command &optional arg &rest _ignored)
  "`company-mode' completion backend for abbrevs accounting for table props.
Respects abbrev table :regexp and :enable-function properties."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'nvp-company-abbrev))
    (prefix (nvp-abbrev-completion-prefix))
    (candidates (nvp-abbrev-completion-candidates arg))
    (meta (abbrev-expansion arg))
    (annotation (or (get-text-property 0 'annotation arg) "<abbrev>"))))

(provide 'nvp-abbrev-completion)
;;; nvp-abbrev-company.el ends here
