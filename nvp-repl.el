;;; nvp-repl.el --- Generic REPL functions -*- lexical-binding: t; -*-

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
  (require 'comint))
(require 'nvp)
(require 'nvp-proc)
(nvp-auto "nvp-sh" nvp-sh-get-process)
(nvp-decls :f (ielm-change-working-buffer))

(cl-defstruct (nvp-repl (:constructor nvp-repl-make))
  "Mode specific REPL variables"
  modes                                 ; REPL major-modes to consider
  bufname                               ; buffer name to search for
  procname                              ; process name to search for
  find-fn                               ; custom function to find REPL
  (live 'process-live-p)                ; check if REPL process is alive
  init                                  ; init REPL => return process
  wait                                  ; time to wait for REPL
  filters                               ; filters applied to text sent to REPL
  (send-fn 'comint-send-string)         ; function to send string to REPL
  cd                                    ; command to change REPL working dir
  cwd                                   ; get current working directory
  ;; set internally
  proc                                  ; REPL process
  buff                                  ; REPL buffer
  )
(put 'nvp-repl-make 'lisp-indent-function 'defun)

(defvar nvp-repl-alist '() "Mapping modes to repls.")

(defvar-local nvp-repl-current () "REPL associated with current buffer.")

;; default REPL to use - shell
(defvar nvp-repl-default (apply #'nvp-repl-make
                                (list :init #'nvp-sh-get-process
                                      :modes '(shell-mode)
                                      :procname "shell"
                                      :bufname "*shell")))

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
  :cd #'ielm-change-working-buffer)

;; may switch storage of REPL vars
(eval-when-compile
  (defmacro nvp-repl--val (val)
    (declare (debug t))
    (or (stringp val) (setq val (symbol-name val)))
    (let ((fn (intern (concat "nvp-repl-" val))))
      `(,fn nvp-repl-current))))

;; return repl for MODE, or default
(defsubst nvp-repl-for-mode (mode)
  (or (cl-loop for (modes . repl) in nvp-repl-alist
         when (memq mode modes)
         return repl)
      nvp-repl-default))

(defun nvp-repl-ensure (&optional mode)
  "Ensure buffer has a REPL associated with MODE or current `major-mode'."
  (or nvp-repl-current
      (setq nvp-repl-current (nvp-repl-for-mode (or mode major-mode)))))

;; -------------------------------------------------------------------
;;; Functions to find REPLs

;;; XXX: this list could be determined when REPL is defined
(defvar nvp-repl-find-functions
  '(nvp-repl-find-custom
    nvp-repl-match-bufname
    nvp-repl-match-procname
    nvp-repl-match-modes)
  "Hook run to find the first applicable REPL process.
Each function takes a process as an argument to test against.")

;; find REPL using a custom function
(defsubst nvp-repl-find-custom ()
  (when-let ((find-fn (nvp-repl--val find-fn)))
    (funcall find-fn)))

;; match process buffer
(defsubst nvp-repl-match-bufname ()
  (when-let ((bname (nvp-repl--val bufname)))
    (nvp-proc-find
     bname :key (lambda (p) (buffer-name (process-buffer p))) :test #'string-match-p)))

;; match process name
(defsubst nvp-repl-match-procname ()
  (when-let ((pname (nvp-repl--val procname)))
    (nvp-proc-find pname :key #'process-name :test #'string-match-p)))

;; match major-mode
(defsubst nvp-repl-match-modes ()
  (when-let ((modes (nvp-repl--val modes)))
    (nvp-proc-find-if
     (lambda (pbuff) (and pbuff (memq (buffer-local-value 'major-mode pbuff) modes)))
     :key #'process-buffer)))

;; -------------------------------------------------------------------
;;; REPL processes

;; associate SRC-BUFFER with REPL
(defsubst nvp-repl--attach (src-buffer)
  (let ((proc (nvp-repl--val proc)))
    (when (and proc (not (process-get proc :src-buffer)))
      (process-put proc :src-buffer src-buffer))))

;; non-nil if PROC is alive
(defsubst nvp-repl-live-p (proc)
  (and proc (funcall (nvp-repl--val live) proc)))

;; check REPL has an associated process and it is alive
;; if it had a proc that died, this updates its proc to nil
;; returns the live process when available
(defsubst nvp-repl-process ()
  (when-let ((proc (nvp-repl--val proc)))
    (if (funcall (nvp-repl--val live) proc) proc
      (setf (nvp-repl--val proc) nil))))

;; get REPL buffer if it has a live process
(defsubst nvp-repl-buffer ()
  (when-let ((proc (nvp-repl-process)))
    (process-buffer proc)))

;; update REPLs proc and procs src-buffer property
(defsubst nvp-repl-update (proc)
  (setf (nvp-repl--val proc) proc
        (nvp-repl--val buff) (process-buffer proc))
  (nvp-repl--attach (current-buffer)))

;; -------------------------------------------------------------------
;;; Initialize new REPLs

(defun nvp-repl-get-buffer ()
  "Return a REPL buffer if one exists, otherwise attempt to start one."
  (nvp-repl-ensure)
  (or (nvp-repl-buffer)
      (when-let ((proc (run-hook-with-args-until-success 'nvp-repl-find-functions))
                 (buff (if (processp proc) (process-buffer proc) proc)))
        (when (nvp-repl-live-p proc)
          ;; found an unregistered live one
          (nvp-repl-update proc)
          buff))
      ;; initialize a new REPL
      (or (nvp-repl-start)
          (user-error "Failed to initialize REPL"))))

(defun nvp-repl-start ()
  "Initialize and return a new REPL buffer associated with current buffer."
  (let ((wait (nvp-repl--val wait))
        (proc-or-buff (funcall (nvp-repl--val init))))
    (and wait (sit-for wait))
    (and (bufferp proc-or-buff)
         (setq proc-or-buff (get-buffer-process proc-or-buff)))
    (nvp-repl-update proc-or-buff)
    (nvp-repl--val buff)))


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
  '((display-buffer-use-some-window
     display-buffer-pop-up-window)
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
  (let ((buff (if-let ((proc (nvp-buffer-process)))
                  (let ((src-buf (process-get proc :src-buffer)))
                    (or (and (buffer-live-p src-buf) src-buf)
                        (other-buffer (current-buffer) 'visible)))
                (nvp-repl-get-buffer))))
    (pop-to-buffer buff nvp-repl--display-action)))

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
