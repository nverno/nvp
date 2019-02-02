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
;;; Tabulate strings in region

;; Print counts of strings in region, with prefix dump at point
;;;###autoload
(defun nvp-stats-uniq (beg end &optional count-lines)
  "Print counts (case-insensitive) of unique words in region BEG to END.
With prefix COUNT-LINES count unique lines."
  (interactive "r\nP")
  (require 'nvp-hash)
  (let ((ht (make-hash-table :test 'case-fold))
        (lines (split-string
                (buffer-substring-no-properties beg end) "\n" 'omit-nulls " "))
        lst)
    (if count-lines
        (dolist (line lines)
          (puthash line (1+ (gethash line ht 0)) ht))
      ;; strip punctuation for words
      (cl-loop for line in lines
         as words = (split-string line "[[:punct:] \t]" 'omit " ")
         when words
         do (cl-loop for word in words
               do (puthash word (1+ (gethash word ht 0)) ht))))
    (maphash (lambda (key val) (push (cons val key) lst)) ht)
    (setq lst (cl-sort lst #'> :key #'car))
    (nvp-with-results-buffer nil
      (pcase-dolist (`(,k . ,v) lst)
        (princ (format "%d: %s\n" k v))))))

(provide 'nvp-stats)
;;; nvp-stats.el ends here
