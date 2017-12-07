;;; nvp-stats ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created:  6 December 2017

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

;; -------------------------------------------------------------------
;;; Util

(eval-when-compile
  (defmacro with-stats-buffer (&rest body)
    (declare (indent defun))
    `(with-current-buffer (get-buffer-create "*nvp-stats*")
       (setq-local buffer-read-only nil)
       ,@body
       (view-mode-enter))))

;; -------------------------------------------------------------------
;;; Tabulate strings in region

;; See the emacs manual for creating a hash table test
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Hash.html
(defun case-fold-string= (a b)
  (eq t (compare-strings a nil nil b nil nil t)))
(defun case-fold-string-hash (a)
  (sxhash (upcase a)))

(define-hash-table-test 'case-fold 'case-fold-string= 'case-fold-string-hash)

;; Print counts of strings in region, with prefix dump at point
;;;###autoload
(defun nvp-stats-uniq (beg end &optional arg)
  (interactive "r\nP")
  (let ((h (make-hash-table :test 'case-fold))
        (strs (split-string (buffer-substring-no-properties beg end) "\n"
                           'omit-nulls " "))
        lst)
    (dolist (str strs) 
      (puthash str (1+ (gethash str h 0)) h))
    (maphash (lambda (key val) (push (cons val key) lst)) h)
    (setq lst (cl-sort lst #'> :key #'car))
    (with-stats-buffer
      (erase-buffer)
      (dolist (pair lst)
        (insert (format "%d: %s\n" (car pair) (cdr pair))))
      (display-buffer (current-buffer)))))

(provide 'nvp-stats)
;;; nvp-stats.el ends here
