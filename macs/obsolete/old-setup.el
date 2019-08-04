
(defmacro nvp-cache-file-path (filename)
  "Create cache path for FILENAME."
  `(expand-file-name ,(nvp-stringify filename) nvp/cache))

;; -------------------------------------------------------------------
;;; Mode bind

(defmacro nvp-mode-bind-1 (&optional mode &rest bindings)
  "Attach BINDINGS globally to MODE."
  (declare (indent defun) (debug bindings))
  (or mode (setq mode (quote major-mode)))
  (unless (equal 'quote (car-safe mode)) (setq mode `',mode))
  `(if (not (get ,mode 'nvp))
       (put ,mode 'nvp ,@bindings)
     (put ,mode 'nvp
          (cl-delete-duplicates
           (append (get ,mode 'nvp) ,@bindings) :key #'car))))

(defmacro nvp-mode-bind (&optional modes &rest bindings)
  "Attach BINDINGS globally to MODES."
  (declare (indent defun) (debug t))
  ;; Allow for some various forms
  (unless (null (cadr (car-safe bindings)))
    (and (equal 'quote (car-safe modes))
         (setq modes (cadr modes)))                       ; '(a b c)
    (unless (listp modes) (setq modes (cons modes nil)))  ; some-mode
    (setq modes (remq 'quote modes))                      ; ('mode-1 'mode-...)
    (macroexp-progn
     (cl-loop for mode in modes
        collect `(nvp-mode-bind-1 ,mode ,@bindings)))))
