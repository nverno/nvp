;;; nvp-hap-python.el --- python help-at-point -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'anaconda-mode nil t)
(require 'nvp)
(require 'nvp-hap)
(nvp:decls :p "anaconda")

(defvar nvp-anaconda-doc-buffer "*Anaconda*")

(defun nvp-anaconda-mode-show-doc-callback (result)
  "Process view doc RESULT."
  (if (> (length result) 0)
      (anaconda-mode-documentation-view result)
    (message "No documentation available")))

;;;###autoload
(defun nvp-hap-python-anaconda (command &optional _arg &rest _args)
  (cl-case command
    (thingatpt (thing-at-point 'symbol))
    (doc-buffer
     (save-window-excursion
       (let ((display-buffer-overriding-action '(nil . ((inhibit-switch-frame . t)))))
         ;; (with-current-buffer nvp-anaconda-doc-buffer
         ;;   (erase-buffer))
         (anaconda-mode-call "show_doc" 'nvp-anaconda-mode-show-doc-callback)
         (--when-let (get-buffer nvp-anaconda-doc-buffer)
           (when (buffer-live-p it)
             (with-current-buffer it
               (list (current-buffer) (pos-bol) nil)))))))))

(provide 'nvp-hap-python)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-hap-python.el ends here
