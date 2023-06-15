;;; nvp-macs-display.el --- display macros -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(require 'nvp-macs-common)

(eval-and-compile
  (defmacro nvp:display-get-action (action type)
    (and (consp action) (setq action (prefix-numeric-value action)))
    `(or (,(if (eq type :buffer) 'cdr 'cadr)
          (assq ,action (plist-get nvp-display-actions ,type)))
         nvp-display-fallback-function)))

(defmacro nvp:display-with-action (action &rest body)
  "Execute BODY with jump ACTION defaults."
  (declare (indent defun) (debug (sexp &rest form)))
  (macroexp-let2 nil action action
    `(let* ((display-buffer-overriding-action
             (nvp:display-get-action ,action :buffer))
            (file-fn (nvp:display-get-action ,action :file))
            (ido-default-file-method (nvp:display-get-action ,action :ido))
            (ido-default-buffer-method ido-default-file-method))
       (nvp:with-letf 'find-file file-fn ,@body))))

; (defmacro nvp:display-file-with-action (action &rest body)
;   "Execute BODY with jump ACTION file defaults."
;   (declare (indent defun) (debug (sexp &rest form)))
;   (macroexp-let2 nil action action
;     `(let* ((file-fn (nvp:display-get-action ,action :file))
;             (ido-default-file-method (nvp:display-get-action ,action :ido)))
;        (cl-letf (((symbol-function 'find-file)
;                   (symbol-function file-fn)))
;          ,@body))))

(defmacro nvp:display-buffer-with-action (action &rest body)
  (declare (indent defun) (debug (sexp &rest form)))
  (macroexp-let2 nil action action
    `(let* ((display-buffer-overriding-action
             (nvp:display-get-action ,action :buffer))
            (ido-default-buffer-method (nvp:display-get-action ,action :ido))
            (help-window-select 'other))
       ,@body)))


(provide 'nvp-macs-display)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-macs-display.el ends here
