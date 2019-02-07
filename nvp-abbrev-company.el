;;; nvp-abbrev-company.el --- local abbrev completion -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-07 01:40:06>
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
             (cons 'local-abbrev-table
              (cons 'global-abbrev-table
                    (nvp-abbrev--all-parents local-abbrev-table)))))))

(defun nvp-abbrev-completion--prefix ()
  (dolist (tab (nvp-abbrev-completion--tables))
    (if-let ((re (abbrev-table-get :regexp)))
        (and (looking-back re (line-beginning-position))))))

;; Return completion candidates, taking into account per-table :regexp
(defun nvp-abbrev-completion--candidates ()
  (interactive)
  (let ((tables (nvp-abbrev-completion--tables))
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
;;; Company

(defun nvp-company-abbrev-prefix ()
  (let ((res (dolist (t (nvp-abbrev-completion--tables))
               ())))))

;;;###autoload
(defun nvp-company-abbrev (command &optional arg &rest _ignored)
  "`company-mode' completion backend for abbrevs accounting for table :regexp."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-abbrev
                                        'company-abbrev-insert))
    (prefix (or (abbrev-table-get local-abbrev-table :regexp)
                (company-grab-symbol)))
    (candidates)))

(provide 'nvp-abbrev-complete)
;;; nvp-abbrev-company.el ends here
