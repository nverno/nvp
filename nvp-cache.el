;;; nvp-cache.el --- Caching helpers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-04-01.23>
;; Created: 25 November 2016

;;; Commentary:

;; FIXME: unused
;; Hash table with expiring entries:
;; https://github.com/skeeto/skewer-mode/blob/master/cache-table.el

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))

(defvar nvp-ring-default-size 50)
(defvar nvp-ring-separator "\n")

(defsubst nvp-ring--extend (ring size)
  (ring-extend ring (min size (- nvp-ring-default-size size))))

(defun nvp-ring-insert (ring item)
  (let ((size (ring-size ring)))
    (and (= size (ring-length ring))
         (nvp-ring--extend ring size)))
  (ring-insert ring item))

(defun nvp-ring-read (filename &optional size silent)
  "Read and return ring from FILENAME."
  (cond
   ((not (file-writable-p filename))
    (or silent
        (message "Cant read %S" filename)))
   (t
    (let* ((count 0)
           (size (min nvp-ring-default-size (or size nvp-ring-default-size)))
           (ring (make-ring size)))
      (with-temp-buffer
        (insert-file-contents filename)
        (goto-char (point-max))
        (let (start end history)
          (while (and (< count size)
                      (re-search-backward nvp-ring-separator nil t)
                      (setq end (match-end 0)))
            (setq start
                  (if (re-search-backward nvp-ring-separator nil t)
                      (match-end 0)
                    (point-min)))
            (setq history (nvp-s 'bs start end))
            (goto-char start)
            ;; skip dupes
            (when (not (string= (ring-ref ring 0) history))
              (when (= count size)
                (nvp-ring--extend ring size)
                (setq size (ring-size ring)))
              (ring-insert-at-beginning ring history)
              (cl-incf count))))
        ring)))))

;; save ring to files
(defun nvp-ring-write (ring filename &optional silent)
  (cond
   ((ring-empty-p ring) nil)
   ((not (file-writable-p filename))
    (or silent
        (user-error "cant write to %S" filename)))
   (t
    (let ((idx (ring-length ring)))
      (with-temp-buffer
        (while (> idx 0)
          (cl-decf idx)
          (insert (ring-ref ring idx) nvp-ring-separator))
        (write-region (buffer-string) nil filename nil 'no-message))))))

;; -------------------------------------------------------------------
;;; Unused 

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
