;;; nvp-cache.el --- Caching helpers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-01-16 20:17:36>
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
;; Hash table with expiring entries:
;; https://github.com/skeeto/skewer-mode/blob/master/cache-table.el
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))

(cl-defstruct (nvp-cache (:constructor nvp-cache--create))
  "Hash table to store cache."
  table default)

(defun nvp-cache-create (&rest args)
  "Create cache table, ARGS passed to `make-hash-table'."
  (nvp-cache--create :table (apply #'make-hash-table args)))

(defun nvp-cache-get (key cache &optional default)
  "Retrieve KEY value from CACHE."
  (cdr (gethash key (nvp-cache-table cache) (or (nvp-cache-default cache)
                                                default))))

(gv-define-setter nvp-cache-get (val key cache)
  "Add KEY-VAL pair to CACHE using `setf'."
  (progn (puthash key val) (nvp-cache-table cache)))

(defun nvp-cache-map (f cache)
  "Apply function F to elements of CACHE ala `maphash'."
  (maphash (lambda (k v) (funcall f k (cdr v))) (nvp-cache-table cache)))

(defun nvp-cache-count (cache)
  "Return number of CACHE entries."
  (hash-table-count (nvp-cache-table cache)))

(defun nvp-cache-to-alist (cache)
  "Return cache as alist."
  (cl-loop with cache = (nvp-cache-table cache)
     for k being the hash-keys of cache using (hash-value v)
     collect (k . v)))

(defun nvp-cache-from-alist (alist &rest options)
  "Build hashtable from values in association list ALIST."
  (let ((ht (apply 'make-hash-table options)))
    (mapc
     (lambda (item) (puthash (car item) (cdr item) ht))
     alist)
    ht))

(defun nvp-cache-save (hash file)
  "Save hashtable HASH as association list to FILE."
  (with-temp-buffer
    (let (print-level print-length)
      (insert (pp-to-string (nvp-cache-to-alist hash)))
      (write-region (point-min) (point-max) file))))

(defun nvp-cache-load (file &optional test)
  (nvp-cache-from-alist
   (with-temp-buffer
     (insert-file-contents file)
     (car (read-from-string (buffer-substring-no-properties
                             (point-min) (point-max)))))
   :test (or test 'equal)))

(provide 'nvp-cache)
;;; nvp-cache.el ends here
