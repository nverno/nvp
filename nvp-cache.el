;;; nvp-cache.el --- caches -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Creates cache with expiring entries.
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp-decls)

(eval-when-compile
  (defsubst nvp-cache-timer-expired-p (lim)
   `(lambda (start) (> (time-to-seconds (- (float-time) start)) ,lim)))

  (defsubst nvp-cache-timer-start () '(lambda (&rest _) (float-time)))

  (defconst nvp-cache--args '(:timeout :expires-fn :expired-p :default))

  (defsubst nvp-cache--clean-args (kwargs)
    (cl-loop for (k v) on kwargs by #'cddr
             unless (memq k nvp-cache--args)
             nconc (list k v))))

(cl-defmacro nvp-cache (&rest kwargs &key timeout expires-fn expired-p default
                              &allow-other-keys)
  "Create cache.
If TIMEOUT is a number, entries expire after that many seconds have elapsed
since the entry was added. Otherwise, EXPIRES-FN and EXPIRED-P are used to
create expiration and test if entries are expired respectively. DEFAULT is
returned by `nvp-cache-get' when a key is absent. Further arguments are
passed to `make-hash-table', with a default \\=':test of \\='equal unless
specified."
  (let ((args (nvp-cache--clean-args kwargs)))
    (cond
     ((and timeout (numberp timeout))
      ;; entries expire after time
      `(nvp-cache-create
        ,(nvp-cache-timer-expired-p timeout)
        ,(nvp-cache-timer-start)
        ,default
        ,@args))
     ((and expires-fn expired-p)
      `(nvp-cache-create ,expired-p ,expires-fn ,default ,@args))
     ((or expires-fn expired-p)
      (error "Cache: need both EXPIRES-FN and EXPIRED-P"))
     (t (error "Cache: no expiration recognized")))))

;;; Cache with expiring entries
(cl-defstruct (nvp-cache (:constructor nvp-cache--create)
                         (:copier nil))
  expires-fn        ; call (expires-fn new-key new-val) to create expiration 
  expired-p         ; call (expired-p expiration) to determine if invalide
  table             ; hash-table
  default)          ; default for `nvp-cache-get'
  
(defun nvp-cache-create (expired-p expires-fn default &rest keyword-args)
  "Create cache table whose entries are invalidated when EXPIRED-P returns
non-nil."
  (unless (cl-getf keyword-args :test nil)
    (setq keyword-args (nconc keyword-args (list ':test 'equal))))
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

(define-inline nvp-cache-clear (cache)
  (inline-quote (clrhash (nvp-cache-table ,cache))))

(provide 'nvp-cache)
;;; nvp-cache.el ends here
