;;; nvp-display.el --- buffer/file display config -*- lexical-binding: t; -*-

;;; Commentary:

;; default configurations for displaying buffers/files

;;; Code:
(eval-when-compile
  (require 'nvp-macro))
(require 'nvp)                          ;nvp-display-actions
(nvp-decl yas-expand-snippet yas-lookup-snippet find-function-other-window)

(defvar nvp-display-fallback-function #'dired "Fallback for unhandled prefix.")

(eval-and-compile
  (defmacro nvp-display:get-action (action type)
    (and (consp action) (setq action (prefix-numeric-value action)))
    `(or (,(if (eq type :buffer) 'cdr 'cadr)
          (assq ,action (plist-get nvp-display-actions ,type)))
         nvp-display-fallback-function)))

(defsubst nvp-display-init-template (template &optional mode start end &rest bindings)
  "Use TEMPLATE to init a new file.
MODE and BINDINGS are passed to `yas-expand-snippet'."
  (cl-progv (mapcar #'car bindings) (mapcar #'cadr bindings)
    (yas-expand-snippet
     (yas-lookup-snippet template mode) start end)))

;; actions to take jumping to buffers/files
;; 4 => same window
;; _ => other window (default)
;;;###autoload
(cl-defun nvp-display-location (location type action &key find-fn init-fn)
  "Display LOCATION of TYPE using ACTION.
Currently supported TYPEs are :buffer, :find-func, :file, and :ido.
Action decides how to display location: 
  - with prefix => same window
  - otherwise   => other window (default)
In INIT-FN is non-nil and LOCATION is a new-file, call INIT-FN."
  (if (not action) (setq action 1)
    (if (consp action) (setq action (prefix-numeric-value action))))
  (pcase (cons type action)
    (`(:buffer . ,_)
     (pop-to-buffer location (nvp-display:get-action action :buffer))
     ;; (and (marker-position (mark-marker))
     ;;      (goto-char (mark-marker)))
     )
    (`(,:find-func . ,_)
     (and (nvp-prefix action (setq action 1) :test '>=))
     (funcall (nvp-display:get-action action type) location))
    (`(,:file . ,_)
     (funcall (nvp-display:get-action action type) location))
    (`(:ido . ,_)
     (let* ((ido-default-file-method (nvp-display:get-action action :ido))
            (ido-default-buffer-method ido-default-file-method))
       (if find-fn (funcall find-fn location)
         (user-error "Which ido to call for %S" location))))
    ((pred functionp) (funcall action location))
    (_ (user-error "Unhandled action(%S)/type(%S) combo" action type)))
  (when (and init-fn (ignore-errors (not (file-exists-p location))))
    (funcall init-fn)))

(defmacro nvp-display-with-action (action &rest body)
  "Execute BODY with jump ACTION defaults."
  (declare (indent defun) (debug (sexp &rest form)))
  (macroexp-let2 nil action action
    `(let* ((display-buffer-overriding-action
             (nvp-display:get-action ,action :buffer))
            (file-fn (nvp-display:get-action ,action :file))
            (ido-default-file-method (nvp-display:get-action ,action :ido))
            (ido-default-buffer-method ido-default-file-method))
       (cl-letf (((symbol-function 'find-file)
                  (symbol-function file-fn)))
         ,@body))))

; (defmacro nvp-display-file-with-action (action &rest body)
;   "Execute BODY with jump ACTION file defaults."
;   (declare (indent defun) (debug (sexp &rest form)))
;   (macroexp-let2 nil action action
;     `(let* ((file-fn (nvp-display:get-action ,action :file))
;             (ido-default-file-method (nvp-display:get-action ,action :ido)))
;        (cl-letf (((symbol-function 'find-file)
;                   (symbol-function file-fn)))
;          ,@body))))

(defmacro nvp-display-buffer-with-action (action &rest body)
  (declare (indent defun) (debug (sexp &rest form)))
  (macroexp-let2 nil action action
    `(let* ((display-buffer-overriding-action
             (nvp-display:get-action ,action :buffer))
            (ido-default-buffer-method (nvp-display:get-action ,action :ido))
            (help-window-select 'other))
       ,@body)))

(provide 'nvp-display)
;;; nvp-display.el ends here
