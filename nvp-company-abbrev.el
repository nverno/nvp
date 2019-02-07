;;; nvp-company-abbrev.el --- company backend for abbrevs -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-07 12:01:25>
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

;; `company-abbrev' doesn't account for :regexp properties or :enable-function,
;; so doesn't work properly in context, eg. ie "\\degree" or "#inc"

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'abbrev)
(require 'company)
(require 'nvp-abbrev-completion)

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

(provide 'nvp-company-abbrev)
;;; nvp-company-abbrev.el ends here
