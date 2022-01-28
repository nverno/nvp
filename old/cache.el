;;; nvp-cache.el --- Generic caches -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Experimenting with defstructs/generics -- nothing particularly useful
;; came from it.
;;
;; Current cache backends:
;; - ring
;; - hash
;;
;; Hash table with expiring entries:
;; https://github.com/skeeto/skewer-mode/blob/master/cache-table.el
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls)

;;; Hash tests

;; case-insensitive hash-table
(define-hash-table-test 'case-fold #'case-fold-string= #'case-fold-string-hash)


;;; Cache
(cl-defstruct (nvp-cache (:constructor nvp-cache-make)
                         (:copier nil))
  data                                ;data structure storing cache
  filename                            ;location to persist cache
  save-predicate                      ;determines if to save entry
  (insert-filter #'identity)          ;applied to elems prior to insertion
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


;;;###autoload
(cl-defun nvp-cache-create (backend &rest args)
  "Create a generic cache."
  (let (cache)
   (pcase-exhaustive backend
     (`ring
      (setq cache (apply #'nvp-cache-ring-make args))
      (setf (nvp-cache-data cache)
            (make-ring (nvp-cache-default-size cache))))
     (`hash
      (let ((test (cl-getf args :test 'equal))
            (weakness (cl-getf args :weakness nil))
            (size (cl-getf args :default-size 65)))
        (cl-remf args :test)
        (cl-remf args :weakness)
        (setq cache (apply #'nvp-cache-hash-make args))
        (setf (nvp-cache-data cache)
              (apply #'make-hash-table (list :test test
                                             :weakness weakness
                                             :size size))))))
   (when-let ((fname (nvp-cache-filename cache)))
     (unless (file-name-absolute-p fname)
       (setf (nvp-cache-filename cache) (expand-file-name fname nvp/cache))))
   cache))

(put 'nvp-cache-create 'lisp-indent-function 'defun)


;;; Generic functions
(cl-defgeneric nvp-cache-length (cache)
  "Number of elements in CACHE."
  (declare (pure t) (side-effect-free t)))

(cl-defgeneric nvp-cache-empty-p (cache)
  "Non-nil if CACHE contains no elements."
  (declare (pure t) (side-effect-free t))
  (zerop (nvp-cache-length cache)))

(cl-defgeneric nvp-cache-elt (elt cache &optional _default)
  "Retrieve element from cache."
  (declare (pure t) (side-effect-free t)))

(cl-defgeneric nvp-cache-insert (elt cache &optional val)
  "Insert new element into cache.")

(cl-defgeneric nvp-cache-load (cache &optional filename)
  "Load data into CACHE from its file or FILENAME.")

(cl-defgeneric nvp-cache-save (cache &optional filename)
  "Write CACHE to its file or FILENAME.")

(cl-defgeneric nvp-cache-map (f cache)
  "Apply function F to elements of CACHE.")

(cl-defgeneric nvp-cache-previous (cache)
  "Return previous element from CACHE.")

(cl-defgeneric nvp-cache-next (cache)
  "Return the next element from CACHE.")

(cl-defgeneric nvp-cache-contains-p (elt cache &optional test)
  "Non-nil if CACHE contains ELT."
  (declare (pure t) (side-effect-free t)))


;; -------------------------------------------------------------------
;;; Ring

(cl-defmethod nvp-cache-length ((cache nvp-cache-ring))
  (ring-length (nvp-cache-data cache)))

(cl-defmethod nvp-cache-elt (elt (cache nvp-cache-ring) &optional default)
  "Retrieve element ELT at from ring CACHE."
  (condition-case nil (ring-ref (nvp-cache-data cache) elt)
    (error default)))

(cl-defmethod (setf nvp-cache-elt) (val elt (cache nvp-cache-ring))
  (let* ((ring (nvp-cache-data cache))
         (len (ring-length ring)))
    (if (< elt len)
        (aset (cddr (nvp-cache-data cache)) elt
              (funcall (nvp-cache-insert-filter cache) val))
      (user-error "ring cache doesn't have %d elements." elt))))

(cl-defmethod nvp-cache-insert (elt (cache nvp-cache-ring) &optional _val)
  "Insert element as most recent in ring cache.
Grows ring when necessary."
  (nvp-ring--insert (nvp-cache-data cache) elt (nvp-cache-default-size cache))
  (cl-incf (nvp-cache-ring-idx cache)))

(cl-defmethod nvp-cache-map (f (cache nvp-cache-ring))
  (nvp-ring--map f (nvp-cache-data cache)))

(cl-defmethod nvp-cache-previous ((cache nvp-cache-ring))
  (nvp-ring--next cache 'previous))

(cl-defmethod nvp-cache-next ((cache nvp-cache-ring))
  (nvp-ring--next cache))

(cl-defmethod nvp-cache-contains-p (elt (cache nvp-cache-ring) &optional _test)
  (ring-member (nvp-cache-data cache) elt))

;; TODO:
(cl-defmethod nvp-cache-load ((_cache nvp-cache-ring) &optional _filename)
  nil)

(cl-defmethod nvp-cache-save ((_cache nvp-cache-ring) &optional _filename)
  nil)


;;; Ring helpers
;; wrappers for ring access - not need to check equality for all items in ring
;; if storing the current index
(defun nvp-ring--next (cache &optional previous)
  (when-let ((idx (nvp-ring--next-index cache previous)))
    (setf (nvp-cache-ring-idx cache) idx)
    (nvp-cache-elt idx cache)))

(defsubst nvp-ring--next-index (cache &optional previous)
  (unless (nvp-cache-empty-p cache)
    (funcall (if previous #'ring-minus1 #'ring-plus1)
             (nvp-cache-ring-idx cache)
             (nvp-cache-length cache))))

(defsubst nvp-ring--extend (ring size default-size)
  (ring-extend ring (min size (- default-size size))))

(defun nvp-ring--insert (ring item default-size)
  "Insert new item, growing ring if necessary."
  (let ((size (ring-size ring)))
    (and (= size (ring-length ring))
         (nvp-ring--extend ring size default-size)))
  (ring-insert ring item))

(defun nvp-ring--map (f ring)
  (let ((start (car ring))
        (size (ring-size ring))
        (vect (cddr ring)))
    (dotimes (var (cadr ring))
      (funcall f (aref vect (mod (+ start var) size))))))

(defun nvp-ring-read (filename default-size &optional ring silent separator)
  "Read and return ring from FILENAME."
  (cond
   ((not (file-writable-p filename))
    (or silent (message "Cant read %S" filename)))
   (t
    (let* ((count 0)
           (size default-size)
           (ring (or ring (make-ring size)))
           (sep (or separator "\n")))
      (with-temp-buffer
        (insert-file-contents filename)
        (goto-char (point-max))
        (let (start end history)
          (while (and (< count size)
                      (re-search-backward sep nil t)
                      (setq end (match-end 0)))
            (setq start
                  (if (re-search-backward sep nil t)
                      (match-end 0)
                    (point-min)))
            (setq history (buffer-substring-no-properties start end))
            (goto-char start)
            ;; skip dupes
            (when (not (string= (ring-ref ring 0) history))
              (when (= count size)
                (nvp-ring--extend ring size default-size)
                (setq size (ring-size ring)))
              (ring-insert-at-beginning ring history)
              (cl-incf count))))
        ring)))))

;; Save ring to files
;; If PREDICATE is non-nil, it is called with a single argument, a ring element
;; if it returns non-nil, the element is saved, otherwise it is skipped.
(defun nvp-ring-write (ring filename &optional silent predicate separator)
  (cond
   ((ring-empty-p ring) nil)
   ((not (file-writable-p filename))
    (or silent (message "cant write to %S" filename)))
   (t
    (let ((idx (ring-length ring)) elem)
      (with-temp-buffer
        (while (> idx 0)
          (cl-decf idx)
          (setq elem (ring-ref ring idx))
          (when (or (null predicate) (funcall predicate elem))
            (insert elem (or separator "\n"))))
        (write-region (buffer-string) nil filename nil 'no-message))))))


;; -------------------------------------------------------------------
;;; Hash

(cl-defmethod nvp-cache-length ((cache nvp-cache-hash))
  (hash-table-count (nvp-cache-data cache)))

(cl-defmethod nvp-cache-elt (elt (cache nvp-cache-hash) &optional default)
  "Retrieve KEY's value from hash CACHE."
  (gethash elt (nvp-cache-data cache) default))

(cl-defmethod (setf nvp-cache-elt) (val key (cache nvp-cache-hash))
  (puthash key val (nvp-cache-data cache)))

(cl-defmethod nvp-cache-insert (key (cache nvp-cache-hash) &optional val)
  (puthash key val (nvp-cache-data cache)))

(cl-defmethod nvp-cache-map (f (cache nvp-cache-hash))
  (maphash (lambda (k v) (funcall f k v)) (nvp-cache-data cache)))

(cl-defmethod nvp-cache-contains-p (key (cache nvp-cache-hash) &optional test)
  (cl-member key (hash-table-keys (nvp-cache-data cache)) :test (or test #'equal)))

;; (defun nvp-cache-to-alist (cache)
;;   "Return cache as alist."
;;   (cl-loop with cache = (nvp-cache-table cache)
;;      for k being the hash-keys of cache using (hash-value v)
;;      collect (cons k v)))

;; (defun nvp-cache-from-alist (alist &rest cache-args)
;;   "Build cache using CACHE-ARGS from values in ALIST."
;;   (let ((cache (apply #'nvp-cache-create cache-args)))
;;     (pcase-dolist (`(,k . ,v) alist)
;;       (setf (nvp-cache-get k cache) v))
;;     cache))

(defun nvp-cache--save-lisp (cache file)
  "Save cache as lisp."
  (with-temp-buffer
    (let (print-level print-length)
      (insert ";; -*- no-byte-compile: t -*-\n")
      (prin1 cache (current-buffer))
      (write-file file))))

(defun nvp-cache--read-lisp (file)
  "Load dumped lisp cache."
  (with-temp-buffer
    (insert-file-contents file)
    (car (read-from-string (buffer-string)))))

(provide 'nvp-cache)
;;; nvp-cache.el ends here
