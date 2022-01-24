;;; nvp-cache.el --- Generic caches -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp-decls)

;;; Hash tests

;; case-insensitive hash-table
(define-hash-table-test 'case-fold #'case-fold-string= #'case-fold-string-hash)

(cl-defstruct (nvp-cache (:constructor nvp-cache--create)
                         (:copier nil))
  ;; call (expires-fn new-key new-val) to compute expiry-value
  ;; call (expired-p expiry-value) to determine if entry is invalidated
  table expired-p expires-fn default)

(defun nvp-cache-create (expired-p expires-fn default &rest keyword-args)
  "Create cache table whose entries are invalidated when EXPIRED-P returns
non-nil."
  (nvp-cache--create
   :expired-p expired-p :expires-fn expires-fn :default default
   :table (apply #'make-hash-table keyword-args)))

(define-inline nvp-cache-size (cache)
  (declare (side-effect-free t))
  (inline-quote (hash-table-count (nvp-cache-table ,cache))))

(defun nvp-cache-get (key cache &optional default)
  "Retrieve KEY value from CACHE unless CACHE-EXPIRED-P is non-nil."
  (let ((val (gethash key (nvp-cache-table cache))))
    (if (or (null val)
            (funcall (nvp-cache-expired-p cache) (car val)))
        (or default (nvp-cache-default cache))
      (cdr val))))

(gv-define-setter nvp-cache-get (val key cache)
  `(progn
     (puthash
      ,key (cons (funcall (nvp-cache-expires-fn ,cache) ,key ,val) ,val)
      (nvp-cache-table ,cache))))

;; (defun nvp-tst-expired-p (v)
;;   (> (time-to-seconds (- (float-time) v)) 10))
;; (defun nvp-tst-expires-fn (k v)
;;   (float-time))
;; (defvar nvp-cache-test (nvp-cache-create
;;                          #'nvp-tst-expired-p
;;                          #'nvp-tst-expires-fn
;;                          0
;;                          :test 'equal))
;; (setf (nvp-cache-get "a" nvp-cache-test) 1)
;; (nvp-cache-get "a" nvp-cache-test)

(provide 'nvp-cache)
;;; nvp-cache.el ends here
