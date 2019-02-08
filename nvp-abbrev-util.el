;;; nvp-abbrev-util.el --- general abbrev utils -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-07 18:12:25>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created:  7 February 2019

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
  (require 'nvp-macro))
(require 'abbrev)
(require 'nvp)

;; get the value of the local abbrev table
(defun nvp-abbrev--local-table ()
  (or local-abbrev-table
      (symbol-value (intern (format "%s-abbrev-table" (symbol-name major-mode))))))

;; return list of currently loaded and non-empty abbrev tables
(defsubst nvp-abbrev--nonempty (&optional tables)
  (cl-remove-if
   (lambda (table)
     (abbrev-table-empty-p (symbol-value table)))
   (or tables abbrev-table-name-list)))

;; get list of all table properties, converting parent tables into symbols
(defun nvp-abbrev--get-plist (table)
  (unless (abbrev-table-p table) (setq table (symbol-value table)))
  (when-let* ((sym (obarray-get table ""))
              (props (symbol-plist sym)))
    (cl-loop for (k v) on props by #'cddr
       if (eq :parents k)
       collect (list k (nvp-abbrev--all-parents table 'names))
       else
       collect (list k v))))

;; list of all table parents, recursively
(defun nvp-abbrev--all-parents (table &optional names)
  (unless (abbrev-table-p table) (setq table (symbol-value table)))
  (let ((parents (abbrev-table-get table :parents))
        res)
    (while parents
      (setq res (append res parents))
      (setq parents (car (mapcar (lambda (tab) (abbrev-table-get tab :parents)) parents))))
    (if names (mapcar #'abbrev-table-name res)
      res)))

;; list all active, nonempty tables:
;; - dynamic table, local table, all parents, global table
(defun nvp-abbrev--active-tables (&optional allow-empty)
  (let ((tabs (append (list local-abbrev-table)
                      (nvp-abbrev--all-parents local-abbrev-table)
                      (list global-abbrev-table))))
    (when (and nvp-abbrev-dynamic-table
               (abbrev-table-p nvp-abbrev-dynamic-table))
      (setq tabs (cons nvp-abbrev-dynamic-table tabs)))
    (setq tabs (delete-dups (mapcar #'abbrev-table-name tabs)))
    (if allow-empty tabs
      (nvp-abbrev--nonempty tabs))))

(provide 'nvp-abbrev-util)
;;; nvp-abbrev-util.el ends here
