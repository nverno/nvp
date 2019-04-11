;;; nvp-repl.el --- Generic REPL functions -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-04-11.11>
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
  (require 'comint))
(require 'nvp)
(require 'nvp-proc)
(require 'nvp-display)
(nvp-decl "" nvp-sh-get-process ielm-change-working-buffer)

(eval-and-compile
  (defvar nvp-repl-alist '() "Mapping modes to repls.")

  (cl-defstruct (nvp-repl (:constructor nvp-repl-make))
    "Mode specific REPL variables"
    modes                                 ; REPL major-modes to consider
    bufname                               ; buffer name to search for
    procname                              ; process name to search for
    find-fn                               ; custom function to find REPL
    (live 'comint-check-proc)             ; check if buffer is available
    init                                  ; init REPL => return process
    wait                                  ; time to wait for REPL
    filters                               ; filters applied to text sent to REPL
    cd                                    ; command to change REPL working dir
    ;; set internally
    proc                                  ; REPL process
    buff                                  ; REPL buffer
    )
  (put 'nvp-repl-make 'lisp-indent-function 'defun)


  ;; default REPL to use - shell
  (defvar nvp-repl-default (apply #'nvp-repl-make
                                  (list :init #'nvp-sh-get-process
                                        :modes '(shell-mode)
                                        :procname "shell"
                                        :bufname "*shell")))

  (defvar-local nvp-repl-buffer () "Current REPL buffer associated with source file.")
  (defvar-local nvp-repl nvp-repl-default "REPL associated with current buffer.")

  (cl-defun nvp-repl-add (mmodes &rest args)
    "Create new mappings of major modes MMODES to repl created from ARGS."
    (unless (listp mmodes) (setq mmodes (cons mmodes nil)))
    (let ((repl (apply #'nvp-repl-make args)))
      (setq mmodes (cons mmodes repl))
      (cl-pushnew mmodes nvp-repl-alist :test #'equal)))
  (put 'nvp-repl-add 'lisp-indent-function 'defun)

  ;; initialize some REPLs
  (nvp-repl-add '(emacs-lisp-mode lisp-interaction-mode)
    :init #'ielm
    :modes '(inferior-emacs-lisp-mode)
    :procname "ielm"
    :bufname "*ielm"
    :cd #'ielm-change-working-buffer))

;; may switch storage of REPL vars
(eval-when-compile
  (defmacro nvp-repl--val (val &optional repl)
    (or (stringp val) (setq val (symbol-name val)))
    (let ((fn (intern (concat "nvp-repl-" val))))
      `(,fn (or ,repl nvp-repl nvp-repl-default)))))

;; return repl for MODE, or default
(defsubst nvp-repl-for-mode (mode)
  (or
   (cl-loop for (modes . repl) in nvp-repl-alist
      when (memq mode modes)
      return repl)
   nvp-repl-default))

;; -------------------------------------------------------------------
;;; Functions to find REPLs

(defvar nvp-repl-find-functions
  '(nvp-repl-find-custom
    nvp-repl-match-bufname
    nvp-repl-match-procname
    nvp-repl-match-modes)
  "Hook run to find the first applicable REPL process.
Each function takes a process as an argument to test against.")

;; find REPL using a custom function
(defsubst nvp-repl-find-custom (&optional repl)
  (when-let ((find-fn (nvp-repl--val find-fn repl)))
    (funcall find-fn)))

;; match process buffer
(defsubst nvp-repl-match-bufname (&optional repl)
  (when-let ((bname (nvp-repl--val bufname repl)))
    (nvp-proc-find
     bname :key (lambda (p) (buffer-name (process-buffer p))) :test #'string-match-p)))

;; match process name
(defsubst nvp-repl-match-procname (&optional repl)
  (when-let ((pname (nvp-repl--val procname repl)))
    (nvp-proc-find pname :key #'process-name :test #'string-match-p)))

;; match major-mode
(defsubst nvp-repl-match-modes (&optional repl)
  (when-let ((modes (nvp-repl--val modes repl)))
    (nvp-proc-find-if
     (lambda (pbuff) (and pbuff (memq (buffer-local-value 'major-mode pbuff) modes)))
     :key #'process-buffer)))

;; -------------------------------------------------------------------
;;; REPL processes

;; associate SRC-BUFFER with REPL
(defsubst nvp-repl--attach (src-buffer &optional repl)
  (let ((proc (nvp-repl--val proc repl)))
    (when (and proc (not (process-get proc :src-buffer)))
      (process-put proc :src-buffer src-buffer))))

;; non-nil if PROC is in a running state
(defsubst nvp-repl-live-p (proc &optional repl)
  (and proc (funcall (nvp-repl--val live repl) proc)))

;; check REPL has an associated process and it is alive
;; if it had a proc that died, this updates its proc to nil
;; returns the live process when available
(defsubst nvp-repl-process (&optional repl)
  (when-let ((proc (nvp-repl--val proc repl)))
    (if (funcall (nvp-repl--val live repl) proc)
        proc
      (setf (nvp-repl--val proc repl) nil))))

;; get REPL buffer if it has a live process
(defsubst nvp-repl-buffer (&optional repl)
  (when-let ((proc (nvp-repl-process repl)))
    (process-buffer proc)))

;; update REPLs proc and procs src-buffer property
(defsubst nvp-repl-update (proc &optional repl)
  (setf (nvp-repl--val proc repl) proc)
  (nvp-repl--attach (current-buffer) repl))

;; -------------------------------------------------------------------
;;; Initialize new REPLs

(cl-defgeneric nvp-repl-get-buffer (&optional repl &rest _args)
  "Return a REPL buffer if one exists, otherwise attempt to start one."
  (or (nvp-repl-buffer repl)
      (if-let ((proc (run-hook-with-args-until-success 'nvp-repl-find-functions)))
          (and (bufferp proc) (setq proc (nvp-buffer-process proc)))
          (when (nvp-repl-live-p proc repl)
            ;; found an unregistered live one
            (nvp-repl-update proc repl)
            (process-buffer proc))
        ;; initialize a new REPL
        (or (nvp-repl-start repl)
            (user-error "Failed to initialize REPL")))))

(cl-defgeneric nvp-repl-start (&optional repl &rest _args)
  "Initialize and return a new REPL buffer associated with current buffer."
  (let ((wait (nvp-repl--val wait repl))
        (proc-or-buff (funcall (nvp-repl--val init repl))))
    (and wait (sit-for wait))
    (nvp-repl-update proc-or-buff repl)
    (if (processp proc-or-buff)
        (process-buffer proc-or-buff)
      proc-or-buff)))

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
   (buffer-substring-no-properties (nvp-point 'bol) (nvp-point 'eoll))))

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
