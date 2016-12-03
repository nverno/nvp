;;; nvp --- 

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

;; [![Build Status](https://travis-ci.org/nverno/nvp.svg?branch=master)](https://travis-ci.org/nverno/nvp)

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (defvar nvp-snippet-dir)
  (defvar nvp-abbrev-local-file)
  (defvar nvp-abbrev-local-table))

(defvar nvp--dir nil)
(when load-file-name
  (setq nvp--dir (file-name-directory load-file-name)))

;; ------------------------------------------------------------
;;; Setup

;;;###autoload
(define-obsolete-function-alias 'nvp-utils-setup-local
  'nvp-tools-setup-local)
;;;###autoload
(cl-defun nvp-tools-setup-local
    (name
     &key
     (mode (concat name "-mode"))
     (dir (symbol-value (intern (concat name "-tools--dir"))))
     (snippets (concat "snippets/" (or mode (symbol-name major-mode))))
     (abbr-table mode)
     (abbr-file (concat name "-abbrev-table"))
     (fn nil))
  "Setup local variables for utils."
  (setq-local nvp-snippet-dir (expand-file-name snippets dir))
  (setq-local nvp-abbrev-local-table abbr-table)
  (setq-local nvp-abbrev-local-file (expand-file-name abbr-file dir))
  (ignore-errors (quietly-read-abbrev-file nvp-abbrev-local-file))
  (when fn (funcall fn)))

;;--- Process Buffer -------------------------------------------------

;; strip ctrl-m, multiple newlines
(defun nvp-process-buffer-filter (proc string)
  (replace-regexp-in-string "[\r\n]+$" "\n" string)
  (with-current-buffer (process-buffer proc)
    (insert string)))

(defsubst nvp-process-buffer (&rest _ignored)
  "*nvp-install*"
  ;; (let ((buff (get-buffer-create (or name "*nvp-install*"))))
  ;;   (with-current-buffer buff
  ;;     (set-process-filter)))
  )

(provide 'nvp)
;;; nvp.el ends here
