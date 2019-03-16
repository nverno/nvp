;;; nvp-display.el --- buffer/file display config -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-15 18:45:19>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 21 February 2019

;;; Commentary:

;; default configurations for displaying buffers/files

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro)
  (require 'macroexp)
  (nvp-local-vars))
(require 'nvp)
(declare-function find-function-other-window "find-func")
(nvp-declare "yasnippet" yas-expand-snippet yas-lookup-snippet)

(defvar nvp-display-fallback-function #'dired "Fallback for unhandled prefix.")

;; overrides to display result in current or other window
(defvar nvp-display--actions
  '(
    :buffer ((4 display-buffer-same-window
                ((inhibit-switch-frame . nil))
                ((inhibit-same-window  . nil)))
             (1 display-buffer-pop-up-window
                ((inhibit-same-window  . t))))
    :file ((4 find-file)
           (1 find-file-other-window))
    :ido ((4 raise-frame)
          (1 other-window))
    :find-func ((4 find-function)
                (1 find-function-other-window))))

(eval-and-compile
  (defmacro nvp-display--get-action (action type)
    (and (consp action) (setq action (prefix-numeric-value action)))
    `(or (,(if (eq type :buffer) 'cdr 'cadr)
          (assq ,action (plist-get nvp-display--actions ,type)))
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
     (pop-to-buffer location (nvp-display--get-action action :buffer))
     (and (marker-position (mark-marker))
          (goto-char (mark-marker))))
    ((or `(,:find-func . ,_) `(,:file . ,_))
     (funcall (nvp-display--get-action action type) location))
    (`(:ido . ,_)
     (let* ((ido-default-file-method (nvp-display--get-action action :ido))
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
             (nvp-display--get-action ,action :buffer))
            (file-fn (nvp-display--get-action ,action :file))
            (ido-default-file-method (nvp-display--get-action ,action :ido))
            (ido-default-buffer-method ido-default-file-method))
       (cl-letf (((symbol-function 'find-file)
                  (symbol-function file-fn)))
         ,@body))))

; (defmacro nvp-display-file-with-action (action &rest body)
;   "Execute BODY with jump ACTION file defaults."
;   (declare (indent defun) (debug (sexp &rest form)))
;   (macroexp-let2 nil action action
;     `(let* ((file-fn (nvp-display--get-action ,action :file))
;             (ido-default-file-method (nvp-display--get-action ,action :ido)))
;        (cl-letf (((symbol-function 'find-file)
;                   (symbol-function file-fn)))
;          ,@body))))

(defmacro nvp-display-buffer-with-action (action &rest body)
  (declare (indent defun) (debug (sexp &rest form)))
  (macroexp-let2 nil action action
    `(let* ((display-buffer-overriding-action
             (nvp-display--get-action ,action :buffer))
            (ido-default-buffer-method (nvp-display--get-action ,action :ido))
            (help-window-select 'other))
       ,@body)))

(provide 'nvp-display)
;;; nvp-display.el ends here
