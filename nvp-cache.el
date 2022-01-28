;;; nvp-cache.el --- caches -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Creates cache with expiring entries.
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls)

;;; Cache
(cl-defmacro nvp-cache (&rest kwargs &key expires-fn expired-p default
                              filename &allow-other-keys)
  "Create caches with expiring entries.

  If EXPIRES-FN is \\='timer, entries expire after EXPIRED-P seconds have
elapsed since the entry was added.

  If EXPIRES-FN is \\='modtime, entries expire if their key (a file) has
been modified since they were added. If FILENAME is non-nil, any entry 
added since it has been modified will be invalid.

  Otherwise, EXPIRES-FN and EXPIRED-P are functions to create and initial
expiration value, and to test if entries are expired, respectively.

DEFAULT is returned by `nvp-cache-get' when a key is absent. Further
arguments are passed to `make-hash-table', with a default \\=':test of
\\='equal unless specified."
  (cl-macrolet ((get-modtime (f)
                  `(let ((attr (file-attributes ,f 'integer)))
                     (and attr (nth 5 attr)))))
    (let* ((args (nvp:arglist-remove-kwargs
                  '(:expires-fn :expired-p :default :filename)
                  kwargs))
           ;; Common caches: (1) time limit, (2) file modification
           (typ (if (and (eq (car-safe expires-fn) 'quote)
                         (memq (cadr expires-fn) '(timer modtime)))
                    (cadr expires-fn)
                  'fn))
           (exp-p
            (pcase typ
              (`timer
               ;; entries expire after time limit
               (unless (numberp expired-p)
                 (error "Cache: timer expects EXPIRED-P to be numer of seconds"))
               (lambda (start) (> (time-to-seconds (- (float-time) start)) expired-p)))
              (`modtime
               ;; entries expire when files are modified
               (if filename
                   (lambda (prev) (equal (get-modtime filename) prev))
                 (lambda (prev) (not (equal (get-modtime (car prev)) (cdr prev))))))
              (_ expired-p)))
           (exp-fn
            (pcase typ
              (`timer (lambda (&rest _) (float-time)))
              (`modtime
               (if filename
                   (lambda (_key) (get-modtime filename))
                 (lambda (file) (cons file (get-modtime file)))))
              (_ expires-fn))))
      (unless (and exp-p exp-fn)
        (error "Cache: need both EXPIRES-FN and EXPIRED-P"))
      `(nvp-cache-create
        ',exp-p
        ',exp-fn
        ,default
        ,@args))))

;;; Cache with expiring entries
(cl-defstruct (nvp-cache (:constructor nvp-cache--create)
                         (:copier nil))
  expires-fn        ; call (expires-fn new-key) to create expiration 
  expired-p         ; call (expired-p expiration) to determine if invalide
  table             ; hash-table
  default)          ; default for `nvp-cache-get'

;;;###autoload
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

(define-inline nvp-cache-clear (cache)
  (inline-quote (clrhash (nvp-cache-table ,cache))))

(defun nvp-cache-clear-expired (cache)
  "Remove any expired entries from CACHE."
  (cl-loop with table = (nvp-cache-table cache)
           for key being the hash-keys of table using (hash-value entry)
           for (prev . val) = entry
           when (funcall (nvp-cache-expired-p cache) prev)
           do (remhash key table)))

(defun nvp-cache-get (key cache &optional default)
  "Retrieve KEY value from CACHE unless CACHE-EXPIRED-P is non-nil."
  (let ((val (gethash key (nvp-cache-table cache))) expired)
    (if (or (null val)
            (setq expired (funcall (nvp-cache-expired-p cache) (car val))))
        (progn
          (and expired (remhash key (nvp-cache-table cache)))
          (or default (nvp-cache-default cache)))
      (cdr val))))

(gv-define-setter nvp-cache-get (val key cache)
  `(progn
     (puthash
      ,key (cons (funcall (nvp-cache-expires-fn ,cache) ,key) ,val)
      (nvp-cache-table ,cache))))

(provide 'nvp-cache)
;;; nvp-cache.el ends here
