;;; nvp-repl.el --- Generic REPL functions -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Always switch to REPL in other window by default, same window
;; TODO:
;; - send-dwim (region / previous sexp)
;; - send-defun
;; - redirect output
;;
;; other options:
;; - https://github.com/kaz-yos/eval-in-repl -- meh

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'comint))
(require 'nvp)
(nvp:auto "nvp-sh" 'nvp-sh-get-process)
(nvp:decls :f (ielm-change-working-buffer sh-cd-here))

(cl-defstruct (nvp-repl (:constructor nvp-repl-make))
  "Mode specific REPL variables"
  modes                              ; REPL major-modes to consider
  init                               ; init REPL => return process
  bufname                            ; buffer name to search for
  procname                           ; process name to search for
  find-fn                            ; custom function to find REPL
  wait                               ; time to wait for REPL
  (live       #'process-live-p)      ; check if REPL process is alive
  (buff->proc #'get-buffer-process)  ; get buffer associated with process
  (proc->buff #'process-buffer)      ; process associated w/ buffer
  ;; the rest are related to interaction (not really implemented)
  (send-fn    #'comint-send-string)  ; function to send string to REPL
  filters                            ; filters applied to text sent to REPL
  cd                                 ; command to change REPL working dir
  cwd                                ; get current working directory
  ;; set internally
  proc                               ; REPL process
  buff                               ; REPL output buffer (may not have a proc)
  )
(put 'nvp-repl-make 'lisp-indent-function 'defun)

(defvar-local nvp-repl-current ()
  "REPL associated with current buffer.")
(defvar nvp-repl-alist '()
  "Mapping modes to repls.")

;; map repl buffers -> source buffers
;; Note: repl buffers may not be processes, eg. slime repls
(defvar nvp-repl--process-buffers (make-hash-table))

;; default REPL to use - shell
(defvar nvp-repl-default (apply #'nvp-repl-make
                                (list :init #'nvp-sh-get-process
                                      :modes '(shell-mode)
                                      :procname "shell"
                                      :bufname "*shell"
                                      :cd #'sh-cd-here)))

;;;###autoload
(defun nvp-repl-add (mmodes &rest args)
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
  :wait 0.1
  :cd (lambda ()
        (if (eq major-mode 'inferior-emacs-lisp-mode)
            (--if-let (gethash (current-buffer) nvp-repl--process-buffers)
                (and (buffer-live-p it)
                     (ielm-change-working-buffer it))
              (message "%s not asociated with a source buffer" (current-buffer)))
          (ielm-change-working-buffer (current-buffer)))))

(nvp-repl-add '(sh-mode bats-mode)
  :init #'nvp-sh-get-process
  :modes '(shell-mode)
  :procname "shell"
  :bufname "*shell"
  :cd #'sh-cd-here)

;; return repl for MODE, or default
(defun nvp-repl-for-mode (mode)
  (or (cl-loop for (modes . repl) in nvp-repl-alist
         when (memq mode modes)
         return repl)
      (message "%S not explicitely associated with any REPLs:\
 using default shell" mode)
      nvp-repl-default))

(defun nvp-repl-ensure (&optional mode)
  "Ensure buffer has a REPL associated with MODE or current `major-mode'."
  (or nvp-repl-current
      (setq nvp-repl-current (nvp-repl-for-mode (or mode major-mode)))))

;; may switch storage of REPL vars
(defmacro repl:val (val)
  (declare (debug t))
  (let ((fn (intern (concat "nvp-repl-" val))))
    `(,fn nvp-repl-current)))

;; -------------------------------------------------------------------
;;; Functions to find REPLs

;;; XXX: this list could be determined when REPL is defined
(defvar nvp-repl-find-functions
  '(nvp-repl-find-custom
    nvp-repl-find-bufname
    nvp-repl-find-procname
    nvp-repl-find-modes)
  "Hook run to find the first applicable REPL process.
Each function takes a process as an argument to test against.")

;; find REPL using a custom function
(defun nvp-repl-find-custom ()
  (--when-let (repl:val "find-fn") (funcall it)))

;; match process buffer
(defun nvp-repl-find-bufname ()
  (--when-let (repl:val "bufname")
    (nvp:proc-find it
      :key (lambda (p) (buffer-name (funcall (repl:val "proc->buff") p)))
      :test #'string-match-p)))

;; match process name
(defun nvp-repl-find-procname ()
  (--when-let (repl:val "procname")
    (nvp:proc-find it :key #'process-name :test #'string-match-p)))

;; match major-mode
(defun nvp-repl-find-modes ()
  (--when-let (repl:val "modes")
    (nvp:proc-find-if
     (lambda (p-buf) (and p-buf (memq (buffer-local-value 'major-mode p-buf) it)))
     :key (lambda (p) (funcall (repl:val "proc->buff") p)))))

;; -------------------------------------------------------------------
;;; REPL processes

;; non-nil if PROC is alive
(defsubst nvp-repl-live-p (proc) (funcall (repl:val "live") proc))

;; check REPL has an associated process and it is alive
;; if it had a proc that died, this updates its proc to nil
;; returns the live process when available
(defun nvp-repl-process ()
  (--when-let (repl:val "proc")
    (if (ignore-errors (nvp-repl-live-p it)) it
      (setf (repl:val "proc") nil
            (repl:val "buff") nil))))

;; get REPL buffer if it has a live process
(defun nvp-repl-buffer ()
  (-some->> (nvp-repl-process)
    (funcall (repl:val "proc->buff"))))

;; update REPLs proc/buff and link process-buffer (which may not be an
;; actual process, eg. slime repl) with source buffer
(defun nvp-repl-update (proc src-buff &optional p-buff)
  (nvp:defq p-buff (funcall (repl:val "proc->buff") proc))
  (setf (repl:val "proc") proc
        (repl:val "buff") p-buff)
  (puthash p-buff src-buff nvp-repl--process-buffers))

;; -------------------------------------------------------------------
;;; Initialize new REPLs

(defun nvp-repl-get-buffer ()
  "Return a REPL buffer if one exists, otherwise attempt to start one."
  (nvp-repl-ensure)
  (or (nvp-repl-buffer)
      (when-let ((proc (run-hook-with-args-until-success 'nvp-repl-find-functions))
                 (p-buff (if (processp proc)
                             (funcall (repl:val "proc->buff") proc)
                           proc)))
        (if (nvp-repl-live-p proc)      ; found unregistered live one
            (nvp-repl-update proc (current-buffer) p-buff)
          (remhash p-buff nvp-repl--process-buffers)))
      (or (nvp-repl-start)              ; initialize new REPL
          (user-error "Failed to initialize REPL"))))

(defun nvp-repl-start ()
  "Return a REPL buffer associated with current buffer."
  (let ((wait (repl:val "wait"))
        (proc (funcall (repl:val "init")))
        p-buff)
    (and wait (sit-for wait))
    (unless (processp proc)
      (setq p-buff proc
            proc (funcall (repl:val "buff->proc") p-buff)))
    (cl-assert (processp proc))
    (nvp-repl-update proc (current-buffer) p-buff)
    (repl:val "buff")))


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

;; `display-buffer' action for popping between REPL/source buffers
(defvar nvp-repl--display-action
  '((display-buffer-reuse-window
     display-buffer-use-some-window
     display-buffer-pop-up-window)
    (reusable-frames . visible)
    (inhibit-switch-frame . t)
    (inhibit-same-window  . t)))

;;;###autoload
(defun nvp-repl-jump (&optional _action)
  "Jump between source and REPL buffers, staring if necessary.
If the associated source buffer no longer exists, pop to next visible window.

TODO:
(1) prefix arg  => set REPL's working directory to same as source if possible.
(2) prefix args => Open REPL in project root directory."
  (interactive "P")
  (let ((buff (--if-let (gethash (current-buffer) nvp-repl--process-buffers)
                  (if (buffer-live-p it) it
                    (other-buffer (current-buffer) 'visible))
                (nvp-repl-get-buffer))))
    (pop-to-buffer buff nvp-repl--display-action)))

;; -------------------------------------------------------------------
;;; Basic REPL interaction

(defun nvp-repl-send-string (str)
  (comint-send-string (nvp-repl-process) str))

(defun nvp-repl-send-region (start end)
  (interactive "r")
  (nvp-repl-send-string (buffer-substring-no-properties start end)))

(defun nvp-repl-send-line ()
  (interactive)
  (nvp-repl-send-string
   (buffer-substring-no-properties (nvp:point 'bol) (nvp:point 'eoll))))

(defun nvp-repl-send-buffer ()
  (interactive)
  (nvp-repl-send-string
   (buffer-substring-no-properties (point-min) (point-max))))

(defun nvp-repl-cd-here (&optional dir)
  (interactive "P")
  (when nvp-repl-current
    (--when-let (repl:val "cd")
      (let ((default-directory (if dir dir default-directory)))
        (funcall it)
        (nvp-repl-update (repl:val "proc") (current-buffer))))))

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
