;;; nvp-repl.el --- Generic REPL functions -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-27 18:02:35>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 22 March 2019

;;; Commentary:

;; Always switch to REPL in other window by default, same window
;; TODO:
;; - send-dwim (region / previous sexp)
;; - send-defun
;; - redirect output

;; other options:
;; - https://github.com/kaz-yos/eval-in-repl -- meh

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'subr-x)
  (require 'cl-lib)
  (require 'cc-defs)                    ; c-point
  (require 'comint))
(require 'nvp)
(require 'nvp-display)

(defvar-local nvp-repl-buffer ()
  "Current REPL buffer associated with source file.")

;; -------------------------------------------------------------------
;;; Utils

;; find buffer with matching BUFNAME
(defsubst nvp-repl--match-name-p (bufname buff)
  (string-match-p bufname (buffer-name buff)))

;; find buffer with major mode in MODES
(defsubst nvp-repl--match-mode-p (modes buff)
  (memq (buffer-local-value 'major-mode buff) modes))

;; find buffer either by name or mode, checking both if available or
;; delegating to other matchers
(defun nvp-repl-find-by-name-or-mode ()
  (let ((modes (nvp-mode-local-or-val 'repl-mode 'all))
        (bufname (nvp-mode-local-or-val 'repl-buffer)))
    (cond
     ((and (null bufname) (null modes)) nil)
     ((null modes)
      (cl-find-if (apply-partially #'nvp-repl--match-name-p bufname) (buffer-list)))
     ((null bufname)
      (cl-find-if (apply-partially #'nvp-repl--match-mode-p modes) (buffer-list)))
     (t (cl-loop for buff in (buffer-list)
           when (or (nvp-repl--match-name-p bufname buff)
                    (nvp-repl--match-mode-p modes buff))
           return buff)))))

;; associate SRC-BUFFER with REPL-BUFFER
(defsubst nvp-repl--associate (src-buffer repl-buffer)
  (let ((proc (get-buffer-process repl-buffer)))
    (when (and proc (not (process-get proc :src-buffer)))
      (process-put proc :src-buffer src-buffer))))

;; non-nil if PROC-OR-BUFF is in a running state
(defsubst nvp-repl-live-p (proc-or-buff)
  (and (not (null proc-or-buff))
       (funcall
        (or (nvp-mode-local-or-val 'repl-live-p) #'comint-check-proc)
        proc-or-buff)))

;; get REPL process - may start REPL if there isn't one
(defsubst nvp-repl-process ()
  (when-let ((buff (nvp-repl-get-buffer)))
    (get-buffer-process buff)))

;; find REPL using a custom function
(defsubst nvp-repl-find-custom ()
  (when-let ((finder (nvp-mode-local-or-val 'repl-find-fn)))
    (ignore-errors (funcall finder))))

(defvar nvp-repl-find-functions
  '(nvp-repl-find-custom nvp-repl-find-by-name-or-mode)
  "Hook run to find the first applicable REPL process.")

;; -------------------------------------------------------------------
;;; Generics

(cl-defgeneric nvp-repl-get-buffer ()
  "Return a REPL buffer if one exists, otherwise attempt to start one."
  (if (nvp-repl-live-p nvp-repl-buffer) nvp-repl-buffer
    (setq nvp-repl-buffer
          (run-hook-with-args-until-success 'nvp-repl-find-functions))
    ;; return live REPL buffer
    (if (nvp-repl-live-p nvp-repl-buffer) nvp-repl-buffer
      ;; otherwise need to initialize a new one
      (or (nvp-repl-start)
          (user-error "Failed to initialize REPL")))))

(cl-defgeneric nvp-repl-start ()
  "Initialize and return a new REPL buffer associated with current buffer."
  (let ((wait (nvp-mode-val 'repl-wait))
        (init-fn (nvp-mode-val 'repl-init-fn))
        (src-buff (current-buffer)))
    (setq nvp-repl-buffer (funcall init-fn))
    (and wait (sit-for wait))
    (and (processp nvp-repl-buffer)
         (setq nvp-repl-buffer (process-buffer nvp-repl-buffer)))
    (nvp-repl--associate src-buff nvp-repl-buffer)
    nvp-repl-buffer))

;; -------------------------------------------------------------------
;;; Commands

;; TODO: mode specific commands
;; - indirect call for sending region, eg. in perl lines should be joined,
;;   racket/guile remove declarations, etc.
;; - setting REPL wd
;; - open REPL in project root
;; - region DWIM
;; - send current defun
;; - sending buffer should call indirect hook to clean input

;;;###autoload
(defun nvp-repl-jump (&optional action)
  "Jump between source and REPL buffers, staring if necessary.
If the associated source buffer no longer exists, pop to next visible window.
(1) prefix arg  => set REPL's working directory to same as source if possible.
(2) prefix args => Open REPL in project root directory."
  (interactive "P")
  (let ((buff (if-let ((proc (nvp-buffer-process)))
                  (let ((src-buf (process-get proc :src-buffer)))
                    (or (and (buffer-live-p src-buf) src-buf)
                        (other-buffer (current-buffer) 'visible)))
                (nvp-repl-get-buffer))))
    (nvp-display-location buff :buffer action)))

(defun nvp-repl-send-string (str)
  (comint-send-string (nvp-repl-process) str))

(defun nvp-repl-send-region (start end)
  (interactive "r")
  (nvp-repl-send-string (buffer-substring-no-properties start end)))

(defun nvp-repl-send-line ()
  (interactive)
  (nvp-repl-send-string
   (buffer-substring-no-properties (c-point 'bol) (c-point 'eoll))))

(defun nvp-repl-send-buffer ()
  (interactive)
  (nvp-repl-send-string
   (buffer-substring-no-properties (point-min) (point-max))))

;;; TODO:
;; (defun nvp-repl-send-defun ()
;;   (interactive)
;;   (nvp-repl-send-string
;;    (buffer-substring-no-properties (c-point ))))

(provide 'nvp-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-repl.el ends here
