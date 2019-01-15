;;; nvp-comment --- comment helpers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-01-14 19:15:18>
;; Package-Requires: 
;; Created: 26 December 2018

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
(autoload 'string-trim "subr-x")

(defun nvp-comment-string (str)
  "Wrap STR with modes starting and ending comment delimiters."
  (let ((comment (if (or (derived-mode-p 'c-mode)
                         (memq major-mode '(python-mode))
                         (string= comment-end ""))
                     comment-start
                   (concat comment-start comment-start))))
    (format "%s%s%s" comment str comment-end)))

(defun nvp-comment-start (length &optional start)
  "Create comment string of LENGTH starting with `comment-start' or START.
Accounts for multi-character comments by recycling the second character."
  (ignore-errors
    (let* ((comment (string-trim (or start comment-start)))
           (cont (if (> (length comment) 1)
                     (substring comment 1 2) comment)))
      (concat comment (make-string (max 0 (- length (length comment)))
                                   (string-to-char cont))))))

(defun nvp-comment-continued (length)
  "Make a comment continuation with LENGTH padding concated with `comment-end'."
  (if (and comment-end (not (string= "" comment-end)))
      (if (> (length comment-start) 1)
          (concat (make-string (1- length) ? )
                  (substring comment-start 1 2))
        (make-string length ? ))
    (nvp-comment-start length)))

(defun nvp-comment-end (&optional trim)
  "Return a TRIMmed version `comment-end' or \"\" if not defined."
  (if (bound-and-true-p comment-end)
      (and trim (string-trim comment-end))
    ""))

(provide 'nvp-comment)
;;; nvp-comment.el ends here
