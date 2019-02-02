;;; nvp-abbrev-auto.el --- commands -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-01 21:43:17>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Maintainer: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created:  1 February 2019

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
(require 'nvp)
(require 'nvp-abbrev)

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

;;;###autoload
(defun nvp-abbrev-load-unicode ()
  "Add unicode abbrevs as parent of local abbrev table."
  (interactive)
  (nvp-abbrev-add-parent
   "unicode-abbrev-table"
   (expand-file-name "unicode-abbrev-table" (nvp-package-root))))

;; write abbrev table
;; temporarily rebind `abbrev--write' to write :system abbrevs
;;;###autoload
(defun nvp-abbrev-write-abbrev-table (table file)
  "Write abbrev TABLE to FILE as :system abbrevs."
  (interactive
   (list
    (if current-prefix-arg
        (abbrev-table-name local-abbrev-table)
      (ido-completing-read
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
      (write-abbrev-file file ))))

(provide 'nvp-abbrev-auto)
;;; nvp-abbrev-auto.el ends here
