;;; nvp-cache.el --- Caching helpers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-02-22 18:27:59>
;; Created: 25 November 2016

;;; Commentary:

;; FIXME: unused
;; Hash table with expiring entries:
;; https://github.com/skeeto/skewer-mode/blob/master/cache-table.el

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))

(cl-defstruct (nvp-cache (:constructor nvp-cache--create)
                         (:copier nil))
  "Hash table to store cache."
  table)

(cl-defun nvp-cache-create (&rest args)
  "Create cache table, ARGS passed to `make-hash-table'."
  (nvp-cache--create :table (apply #'make-hash-table args)))

(defun nvp-cache-get (key cache &optional default)
  "Retrieve KEY value from CACHE."
  (gethash key (nvp-cache-table cache) default))

(gv-define-setter nvp-cache-get (val key cache)
  "Add KEY-VAL pair to CACHE using `setf'."
  `(puthash ,key ,val (nvp-cache-table ,cache)))

(defun nvp-cache-map (f cache)
  "Apply function F to elements of CACHE ala `maphash'."
  (maphash (lambda (k v) (funcall f k v)) (nvp-cache-table cache)))

(defun nvp-cache-count (cache)
  "Return number of CACHE entries."
  (hash-table-count (nvp-cache-table cache)))

(defun nvp-cache-to-alist (cache)
  "Return cache as alist."
  (cl-loop with cache = (nvp-cache-table cache)
     for k being the hash-keys of cache using (hash-value v)
     collect (cons k v)))

(defun nvp-cache-from-alist (alist &rest cache-args)
  "Build cache using CACHE-ARGS from values in ALIST."
  (let ((cache (apply #'nvp-cache-create cache-args)))
    (pcase-dolist (`(,k . ,v) alist)
      (setf (nvp-cache-get k cache) v))
    cache))

(defun nvp-cache-save (cache file)
  "Save cache to file."
  (with-temp-buffer
    (let (print-level print-length)
      (insert ";; -*- no-byte-compile: t -*-\n")
      (prin1 cache (current-buffer))
      (write-file file))))

(defun nvp-cache-load (file)
  "Load saved cache."
  (with-temp-buffer
    (insert-file-contents file)
    (car (read-from-string (buffer-string)))))

(provide 'nvp-cache)
;;; nvp-cache.el ends here
