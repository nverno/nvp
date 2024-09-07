;;; nvp-display.el --- buffer/file display config -*- lexical-binding: t; -*-

;;; Commentary:

;; default configurations for displaying buffers/files
;; actions to take jumping to buffers/files
;; nil => other window (default)
;; 4   => same window
;; >4  => call fallback

;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)                          ; nvp-display-actions
(nvp:decls)

(defun nvp-display-init-template (template &optional mode start end &rest bindings)
  "Use TEMPLATE to init a new file.
MODE and BINDINGS are passed to `yas-expand-snippet'."
  (cl-progv (mapcar #'car bindings) (mapcar #'cadr bindings)
    (yas-expand-snippet
     (yas-lookup-snippet template mode) start end)))

;;;###autoload
(defun nvp-display-fallback-dired (location &rest _args)
  (if (bufferp location)
      nil
    (let ((dir (pcase location
                 ((pred file-exists-p)
                  (file-name-directory location))
                 ;; ((pred bufferp)
                 ;;  (file-name-directory (buffer-file-name location)))
                 (_ (-if-let (buff (get-buffer location))
                        (file-name-directory
                         (buffer-file-name location))
                      default-directory)))))
      (dired dir))))

;;;###autoload
(cl-defun nvp-display-location (location type action &key find-fn init-fn)
  "Display LOCATION of TYPE using ACTION.
Currently supported TYPEs are :buffer, :find-func, :file, and :ido.
Action decides how to display location depending on prefix: 
  - none	=> other window (default)
  - \\[universal-argument]		=> same window
  - \\[universal-argument]\\[universal-argument]...	=> fallback
In INIT-FN is non-nil and LOCATION is a new-file, call INIT-FN."
  (if (not action) (setq action 1)
    (if (consp action) (setq action (prefix-numeric-value action))))
  (pcase (cons type action)
    (`(:buffer . ,_) (pop-to-buffer
                      location (nvp-display--action action :buffer)))
    (`(,:find-func . ,_) (funcall (nvp-display--action action type) location))
    (`(,:file . ,_)
     (if-let ((buf (get-file-buffer location)))
         (pop-to-buffer buf (nvp-display--action action :buffer))
       (funcall (nvp-display--action action type) location)))
    (`(:ido . ,_) (let* ((ido-default-file-method
                          (nvp-display--action action :ido))
                         (ido-default-buffer-method ido-default-file-method))
                    (if find-fn
                        (funcall find-fn location)
                      (user-error "Which ido to call for %S" location))))
    ((pred functionp) (funcall action location))
    (_ (user-error "Unhandled action(%S)/type(%S) combo" action type)))
  (when (and init-fn (ignore-errors (not (file-exists-p location))))
    (funcall init-fn)))

(provide 'nvp-display)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-display.el ends here
