;;; nvp-abbrev --- 

;; This is free and unencumbered software released into the public domain.

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
  (require 'nvp-macro)
  (require 'cl-lib))

(defvar-local nvp-abbrev-local-table nil)

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

(defun nvp-abbrev--get-table (table file)
  ;; if (file-exists-p file)
  ;; (find-file-other-window file)
  (find-file-other-window file)
  (goto-char (point-min))
  (unless (search-forward-regexp (concat "'" table "\\>") nil t)
    (goto-char (point-max))
    (nvp-abbrev--insert-template table)))

;; grab preceding characters to match agains in abbrev table
(defun nvp-abbrev--grab-prev (arg)
  (save-excursion
    (let ((end (point))
         (_ (skip-chars-backward "A-Za-z0-9#." (- (point) arg)))
         (start (point)))
     (buffer-substring-no-properties start end))))

;; Jump to abbrev file, search to position to insert (by length, then
;; lex).  Prefix length to look back.
;;;###autoload
(defun nvp-jump-to-mode-abbrev (arg)
  (interactive "p")
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
    (if pref
        (while
            (and
             (re-search-forward "[^\\(?:(define-\\)](\"\\(\\w+\\)"
                                nil t)
             (progn
               (let ((str (buffer-substring-no-properties
                           (match-beginning 1) (match-end 1))))
                 (if (> (length pref) (length str)) t
                   (string> pref str)))))))
    (add-hook 'after-save-hook
              #'(lambda () (quietly-read-abbrev-file buffer-file-name))
              t 'local)))

(provide 'nvp-abbrev)
;;; nvp-abbrev.el ends here
