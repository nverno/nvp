;;; nvp-comint.el --- Comint helpers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-03-28 02:47:05>
;; Created: 31 March 2017

;;; Commentary:

;; TODO:
;; - default `comint-input-filter-functions' to ignore blanks, compress newlines

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib)
  (require 'subr-x))
(require 'comint)

;; kill process before killing buffer -- ensure comint writes history
(defun nvp-comint-kill-proc-before-buffer ()
  (let ((proc (nvp-buffer-process)))
    (when (processp proc)
      (delete-process proc))))

;;; History

;; FIXME: setup hippie expansion for shell at same time as history
;;;###autoload
(defun nvp-comint-setup-history (filename &optional size write-history)
  ;; setup read/write for history file
  (setq comint-input-ignoredups t)      ;FIXME: move to comint-hook
  (setq comint-input-ring-file-name (expand-file-name filename nvp/cache))
  (and size (setq-local comint-input-ring-size size))
  (comint-read-input-ring 'silent)
  (when-let* ((proc (get-buffer-process (current-buffer))))
    (and write-history (nvp-comint-add-history-sentinel proc))))

;; FIXME: this is called too much. 
(defun nvp-comint-add-history-sentinel (&optional proc)
  (when-let* ((proc (or proc (nvp-buffer-process))))
    (add-function :before (process-filter proc) #'nvp-comint-history-sentinel)))

(defun nvp-comint-history-sentinel (proc _m)
  (with-current-buffer (process-buffer proc)
    (comint-write-input-ring)))

;; FIXME: is `nvp-comint-add-history-sentinel' necessary with this hook?
;; write comint-input-ring when buffer is killed: in kill-buffer-hook
(defun nvp-comint-write-history-on-kill ()
  ;; make sure the buffer exists before calling the process sentinel
  (add-hook 'kill-buffer-hook 'nvp-comint-kill-proc-before-buffer nil 'local)
  (advice-add 'nvp-comint-kill-proc-before-buffer :before 'comint-write-input-ring))

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
