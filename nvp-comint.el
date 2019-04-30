;;; nvp-comint.el --- Comint helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; - sentinels
;; - manage history
;; - redirect output

;; TODO:
;; - default `comint-input-filter-functions' to ignore blanks, compress newlines

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro)
  (require 'subr-x))
(require 'comint)
(nvp-decls)

;; kill process before killing buffer -- ensure comint writes history
(defun nvp-comint-kill-proc-before-buffer ()
  (let ((proc (nvp-buffer-process)))
    (when (processp proc)
      (delete-process proc))))

;; -------------------------------------------------------------------
;;; History

;; write comint-input-ring when buffer is killed: in kill-buffer-hook
(defun nvp-comint-write-history-on-kill ()
  ;; make sure the buffer exists before calling the process sentinel
  (add-hook 'kill-buffer-hook 'nvp-comint-kill-proc-before-buffer nil 'local)
  (advice-add 'nvp-comint-kill-proc-before-buffer :before 'comint-write-input-ring))

;; Setup history file and hippie expansion
;;;###autoload
(defun nvp-comint-setup-history (filename &rest args)
  (setq comint-input-ring-file-name (expand-file-name filename nvp/cache))
  (comint-read-input-ring 'silent)
  (apply #'nvp-he-history-setup args))

;; evaluate STRING in PROC, but discard output silently
(defun nvp-comint-redirect-silently (proc string &optional prompt)
  (let ((comint-redirect-perform-sanity-check))
    (with-temp-buffer 
      (comint-redirect-send-command-to-process
       string (current-buffer) proc nil 'no-display)
      ;; wait for process to complete
      (with-current-buffer (process-buffer proc)
        (while (and (null comint-redirect-completed) ;ignore output
                    (accept-process-output proc 1)))))
    (with-current-buffer (process-buffer proc)
      (comint-redirect-cleanup)
      (while (and (null comint-redirect-completed)   ;wait for cleanup to finish
                  (accept-process-output proc 1)))
      (and prompt (comint-send-string proc "\n"))))) ;optionally print a new prompt

(provide 'nvp-comint)
;;; nvp-comint.el ends here
