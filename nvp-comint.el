;;; nvp-comint.el --- Comint helpers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-02-22 18:31:05>
;; Created: 31 March 2017

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (nvp-local-vars)
  (require 'cl-lib)
  (require 'subr-x))
(require 'comint)
(declare-function nvp-inf-kill-proc-before-buffer "nvp-inf")

;; size of history files to save
(defvar nvp-comint-history-size 5000)

;;; History

;;;###autoload
(defun nvp-comint-setup-history (filename &optional size write-history)
  ;; setup read/write for history file
  (setq comint-input-ignoredups t)
  (setq comint-input-ring-file-name (expand-file-name filename nvp/cache))
  (setq-local comint-input-ring-size (or size nvp-comint-history-size))
  (comint-read-input-ring 'silent)
  (when-let* ((proc (get-buffer-process (current-buffer))))
    (and write-history
         (add-function :before (process-filter proc) #'nvp-comint-history-sentinel))))

;; to be called in a hook
(defun nvp-comint-add-history-sentinel ()
  (when-let* ((proc (current-buffer-process)))
    (add-function :before (process-filter proc) #'nvp-comint-history-sentinel)))

;; write comint-input-ring when buffer is killed: in kill-buffer-hook
;;;###autoload
(defun nvp-comint-write-history-on-kill ()
  ;; make sure the buffer exists before calling the process sentinel
  (add-hook 'kill-buffer-hook 'nvp-inf-kill-proc-before-buffer nil 'local)
  (advice-add 'nvp-inf-kill-proc-before-buffer :before 'comint-write-input-ring))

(defun nvp-comint-history-process-sentinel (proc _m)
  (when (not (comint-check-proc proc))
    (comint-write-input-ring)))

;;;###autoload
(defun nvp-comint-history-sentinel (proc _m)
  (with-current-buffer (process-buffer proc)
    (comint-write-input-ring)))

;;;###autoload
(defun nvp-comint-redirect-silently (proc string &optional prompt)
  (let* ((comint-redirect-perform-sanity-check))
    (with-temp-buffer 
      (comint-redirect-send-command-to-process
       string (current-buffer) proc nil 'no-display)
      ;; wait for process to complete
      (set-buffer (process-buffer proc))
      (while (and (null comint-redirect-completed)   ;ignore output
                  (accept-process-output proc 1))))
    (with-current-buffer (process-buffer proc)
      (comint-redirect-cleanup)
      (while (and (null comint-redirect-completed)   ;wait for cleanup to finish
                  (accept-process-output proc 1)))
      (and prompt (comint-send-string proc "\n"))))) ;optionally print a new prompt

(provide 'nvp-comint)
;;; nvp-comint.el ends here
