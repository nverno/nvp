;;; nvp-hash ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 25 November 2016

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

(defun nvp-hash-cache-init (&optional test)
  (make-hash-table :test (or test 'equal)))

(defun nvp-hash-cache-get (key cache)
  (gethash key cache))

(defun nvp-hash-cache-put (key value cache)
  (when value
    (puthash key value cache))
  value)

(defun nvp-hash-to-alist (hash)
  "Convert hashtable to association list."
  (let (res)
    (maphash
     (lambda (key value)
       (setq res (cons (cons key value) res)))
     hash)
    res))

(defun nvp-hash-from-alist (alist &rest options)
  "Build hashtable from values in association list ALIST."
  (let ((ht (apply 'make-hash-table options)))
    (mapc
     (lambda (item) (puthash (car item) (cdr item) ht))
     alist)
    ht))

(defun nvp-hash-cache-save (hash file)
  "Save hashtable HASH as association list to FILE."
  (with-temp-buffer
    (let (print-level print-length)
      (insert (pp-to-string (nvp-hash-to-alist hash)))
      (write-region (point-min) (point-max) file))))

(defun nvp-hash-cache-load (file &optional test)
  (nvp-hash-from-alist
   (with-temp-buffer
     (insert-file-contents file)
     (car (read-from-string (buffer-substring-no-properties
                             (point-min) (point-max)))))
   :test (or test 'equal)))

(provide 'nvp-hash)
;;; nvp-hash.el ends here
