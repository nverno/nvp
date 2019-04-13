;;; nvp-cache.el --- Caching helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; Generic cache

;; FIXME: mostly unused
;; Hash table with expiring entries:
;; https://github.com/skeeto/skewer-mode/blob/master/cache-table.el

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))

;; TODO: get rid of these
(defvar nvp-ring-default-size 50)
(defvar nvp-ring-separator "\n")

(defsubst nvp-cache--names (struct-type)
  (declare (pure t) (side-effect-free t))
  (let* ((class (cl--struct-get-class struct-type))
         (slots (cl--struct-class-slots class)))
    (cl-loop for i across slots
       collect (cl--slot-descriptor-name i))))

(cl-defstruct (nvp-cache (:constructor nvp-cache-make)
                         (:copier nil))
  data                                ;data structure storing cache
  filename                            ;location to persist cache
  predicate                           ;determines if to save entry
  (separator "\n")                    ;separator used when writing entries
  (default-size 65)                   ;default cache size (allowed to grow)
  (silent t))                         ;no read/write messages, eg. fail silently

(cl-defstruct (nvp-cache-ring (:include nvp-cache)
                              (:constructor nvp-cache-ring-make)
                              (:copier nil))
  "Cache using ring store."
  ;; cache using ring backend, idx is current index
  (idx 0))

(cl-defstruct (nvp-cache-hash
               (:include nvp-cache)
               (:constructor nvp-cache-hash-make)
               (:copier nil))
  "Cache using hash table store.")

(cl-defun nvp-cache-create (backend &rest args)
  "Create a generic cache."
  (pcase-exhaustive backend
    (`ring
     (let ((cache (apply #'nvp-cache-ring-make args)))
       (setf (nvp-cache-ring-data cache)
             (make-ring (nvp-cache-ring-default-size cache)))
       cache))
    (`hash
     (let ((test (cl-getf args :test 'equal))
           (weakness (cl-getf args :weakness nil))
           (size (cl-getf args :default-size 65)))
       (cl-remf args :test)
       (cl-remf args :weakness)
       (let ((cache (apply #'nvp-cache-hash-make args)))
        (setf (nvp-cache-hash-data cache)
              (apply #'make-hash-table (list :test test
                                             :weakness weakness
                                             :size size)))
        cache)))))

(cl-defgeneric nvp-cache-length (cache)
  "Number of elements in CACHE.")

(cl-defmethod nvp-cache-length ((cache nvp-cache-ring))
  (ring-length (nvp-cache-ring-data cache)))

(cl-defmethod nvp-cache-length ((cache nvp-cache-hash))
  (hash-table-count (nvp-cache-hash-data cache)))

(cl-defgeneric nvp-cache-empty-p (cache)
  "Non-nil if CACHE contains no elements."
  (zerop (nvp-cache-length cache)))

(cl-defgeneric nvp-cache-elt (elt cache &optional _default)
  "Retrieve element from cache."
  (declare (pure t) (side-effect-free t)))

(cl-defmethod nvp-cache-elt (elt (cache nvp-cache-ring) &optional _default)
  "Retrieve element at IDX from ring CACHE."
  (ring-ref (nvp-cache-ring-data cache) elt))

(cl-defmethod nvp-cache-elt (elt (cache nvp-cache-hash) &optional default)
  "Retrieve KEY's value from hash CACHE."
  (gethash elt (nvp-cache-hash-data cache) default))

(cl-defmethod (setf (cache nvp-cache-elt)) (val cache key)
  (puthash key val (nvp-cache-hash-data cache)))
;; (gv-define-setter nvp-cache-get (key val (cache nvp-cache-hash))
;;   "Add KEY-VAL pair to hash CACHE using `setf'."
;;   `(puthash ,key ,val (nvp-cache-hash-data ,cache)))

(cl-defgeneric nvp-cache-map (f cache)
  "Apply function F to elements of CACHE.")

(cl-defmethod nvp-cache-map (f (cache nvp-cache-hash))
  (maphash (lambda (k v) (funcall f k v)) (nvp-cache-hash-data cache)))

(defun nvp-cache-count (cache)
  "Return number of CACHE entries."
  (hash-table-count (nvp-cache-table cache)))

;; -------------------------------------------------------------------
;;; Ring

(defsubst nvp-ring-ref (ring)
  (ring-ref (nvp-ring-ring ring) (nvp-ring-idx ring)))
(defsubst nvp-ring--extend (ring size)
  (ring-extend ring (min size (- nvp-ring-default-size size))))

;; wrappers for ring access - not need to check equality for all items in ring
;; if storing the current index
(defsubst nvp-ring-next-index (ring idx)
  (ring-plus1 idx (ring-length ring)))

(defsubst nvp-ring-previous-index (ring idx)
  (ring-minus1 idx (ring-length ring)))

(defun nvp-ring-insert (ring item)
  "Insert new item, growing ring if necessary."
  (let ((size (ring-size ring)))
    (and (= size (ring-length ring))
         (nvp-ring--extend ring size)))
  (ring-insert ring item))

(defun nvp-ring-read (filename &optional size silent)
  "Read and return ring from FILENAME."
  (cond
   ((not (file-writable-p filename))
    (or silent (message "Cant read %S" filename)))
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

;; Save ring to files
;; If PREDICATE is non-nil, it is called with a single argument, a ring element
;; if it returns non-nil, the element is saved, otherwise it is skipped.
(defun nvp-ring-write (ring filename &optional silent predicate)
  (cond
   ((ring-empty-p ring) nil)
   ((not (file-writable-p filename))
    (or silent (message "cant write to %S" filename)))
   (t
    (let ((idx (ring-length ring)))
      (with-temp-buffer
        (while (> idx 0)
          (cl-decf idx)
          (when (or (null predicate) (funcall predicate (ring-ref ring idx)))
            (insert (ring-ref ring idx) nvp-ring-separator)))
        (write-region (buffer-string) nil filename nil 'no-message))))))

;; -------------------------------------------------------------------
;;; Hash
;; TODO: generalize

(defun nvp-cache-to-alist (cache)
  "Return cache as alist."
  (cl-loop with cache = (nvp-cache-table cache)
     for k being the hash-keys of cache using (hash-value v)
     collect (cons k v)))

;; (defun nvp-cache-from-alist (alist &rest cache-args)
;;   "Build cache using CACHE-ARGS from values in ALIST."
;;   (let ((cache (apply #'nvp-cache-create cache-args)))
;;     (pcase-dolist (`(,k . ,v) alist)
;;       (setf (nvp-cache-get k cache) v))
;;     cache))

(defun nvp-cache-map (f cache)
  "Apply function F to elements of CACHE ala `maphash'."
  (maphash (lambda (k v) (funcall f k v)) (nvp-cache-table cache)))

(defun nvp-cache-count (cache)
  "Return number of CACHE entries."
  (hash-table-count (nvp-cache-table cache)))

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
