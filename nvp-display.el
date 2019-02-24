;;; nvp-display.el --- buffer/file display config -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-22 19:25:54>
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

;; overrides to display result in current or other window
(defvar nvp-display--actions
  '(
    :buffer ((4 display-buffer-same-window
                ((inhibit-switch-frame . nil))
                ((inhibit-same-window  . nil)))
             (t display-buffer-pop-up-window
                ((inhibit-same-window  . t))))
    :file ((4 find-file)
           (t find-file-other-window))
    :ido ((4 raise-frame)
          (t other-window))
    :find-func ((4 find-function)
                (t find-function-other-window))))

(eval-and-compile
  (defmacro nvp-display--get-action (action type)
   `(,(if (eq type :buffer) 'cdr 'cadr)
     (assq ,action (plist-get nvp-display--actions ,type)))))

;; actions to take jumping to buffers/files
;; 4 => same window
;; _ => other window (default)
(defun nvp-display-location (location type action &optional func)
  (or action (setq action t))
  (pcase (cons type action)
    (`(:buffer . ,_)
     (pop-to-buffer location (nvp-display--get-action action :buffer)))
    ((or `(,:find-func . ,_) `(,:file . ,_))
     (funcall (nvp-display--get-action action type) location))
    (`(:ido . ,_)
     (let* ((ido-default-file-method (nvp-display--get-action action :ido))
            (ido-default-buffer-method ido-default-file-method))
       (if func (funcall func location)
         (user-error "Which ido to call for %S" location))))
    ((pred functionp) (funcall action location))
    (_ (user-error "Unhandled action(%S)/type(%S) combo" action type))))

(defmacro nvp-display-with-action (action &rest body)
  "Execute BODY with jump ACTION defaults."
  (declare (indent defun) (debug (sexp &rest form)))
  (macroexp-let2 nil action action
    `(let* ((display-buffer-overriding-action
            (nvp-display--get-action ,action :buffer))
           (file-fn (nvp-display--get-action ,action :file))
           (ido-default-file-method (nvp-display--get-action ,action :ido))
           (ido-default-buffer-method ido-default-file-method))
      (cl-letf (((symbol-function 'find-file) (symbol-function file-fn)))
        ,@body))))

(defmacro nvp-display-file-with-action (action &rest body)
  "Execute BODY with jump ACTION file defaults."
  (declare (indent defun) (debug (sexp &rest form)))
  (macroexp-let2 nil action action
    `(let* ((file-fn (nvp-display--get-action ,action :file))
            (ido-default-file-method (nvp-display--get-action ,action :ido)))
       (cl-letf (((symbol-function 'find-file) (symbol-function file-fn)))
         ,@body))))

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