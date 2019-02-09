;;; nvp-abbrev-completion.el --- local abbrev completion -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-08 22:29:44>
;; Author: Noah Peart <noah.v.peart@gmail.com>
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

;; Completion functions for abbrevs that take into account local context
;; using each active table's :regexp and :enable-function properties
;;
;; These complete for all parents of `local-abbrev-table', accounting for
;; :enable-function and :regexp properties for each table separately
;;
;; Used by company and hippie-exp.
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(require 'abbrev)
(require 'nvp)
(require 'nvp-abbrev-util)

;; if non-nil, update active table cache
(defvar-local nvp-abbrev-completion-need-refresh nil)

;; use local table along with its parents + global table
(defvar-local nvp-abbrev-completion--tables nil)
(defun nvp-abbrev-completion--tables ()
  (or (or nvp-abbrev-completion-need-refresh nvp-abbrev-completion--tables)
      (prog1 (setq nvp-abbrev-completion--tables (nvp-abbrev--active-tables))
        (setq nvp-abbrev-completion-need-refresh nil))))

;; add completion annotations
(defun nvp-abbrev-completion--apply-annotation (table)
  (let ((tab-name (symbol-name table)))
    (mapatoms (lambda (sym) (add-text-properties 0 1 (list 'annotation tab-name) sym)))))

;; active tables in current context, determined by :enable-function
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

;; return beginning position of prefix for hippie
(defun nvp-abbrev-completion-prefix-beg ()
  (and (nvp-abbrev-completion--prefix)
       (match-beginning 0)))

;; return prefix, either matching a table's predicates or defaulting to the
;; previous symbol
(defun nvp-abbrev-completion-prefix ()
  (or (nvp-abbrev-completion--prefix)
      (nvp-grab-symbol)))

;; Return completion candidates, taking into account per-table :regexp
(defun nvp-abbrev-completion-candidates (arg)
  (cl-loop for tab in (nvp-abbrev-completion--active-tables)
     nconc (delete "" (all-completions arg (symbol-value tab)))))

(provide 'nvp-abbrev-completion)
;;; nvp-abbrev-completion.el ends here
