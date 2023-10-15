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

(cl-defstruct (nvp--repl (:constructor nvp-repl-make))
  "Mode specific REPL variables"
  modes                              ; REPL major-modes to consider
  init                               ; init REPL => return process
  init-callback                      ; after init REPL call a callback to link source/repl
  bufname                            ; buffer name to search for
  procname                           ; process name to search for
  find-fn                            ; custom function to find REPL
  wait                               ; time to wait for REPL
  (live       #'process-live-p)      ; check if REPL process is alive
  (buff->proc #'get-buffer-process)  ; get buffer associated with process
  (proc->buff #'process-buffer)      ; process associated w/ buffer
  ;; the rest are related to interaction
  (send-string    #'comint-send-string) ; function to send string to REPL
  (send-input     #'comint-send-input)  ; function to send current input from REPL
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
(defvar nvp-repl-default
  (apply #'nvp-repl-make
         (list :init #'nvp-sh-get-process
               :modes '(shell-mode)
               :procname "shell"
               :bufname "*shell"
               :cd (lambda (&rest _) (funcall-interactively #'sh-cd-here)))))

(defun nvp-repl-remove (mode)
  (interactive (list (intern (nvp-read-mode))))
  (setq nvp-repl-alist
        (-filter (lambda (e)
                   (not (memq mode (car e))))
                 nvp-repl-alist)))

;;;###autoload
(defun nvp-repl-add (mmodes &rest args)
  "Create new mappings of major modes MMODES to repl created from ARGS."
  (unless (listp mmodes) (setq mmodes (cons mmodes nil)))
  (let ((repl (apply #'nvp-repl-make args)))
    (setq mmodes (cons mmodes repl))
    (cl-pushnew mmodes nvp-repl-alist :test #'equal)))
(put 'nvp-repl-add 'lisp-indent-function 'defun)

;; initialize some REPLs
(nvp:decl ielm-send-input)
(nvp-repl-add '(emacs-lisp-mode lisp-interaction-mode)
  :init #'ielm
  :modes '(inferior-emacs-lisp-mode)
  :procname "ielm"
  :bufname "*ielm"
  :send-input #'ielm-send-input
  :wait 0.1
  :cd (lambda ()
        (if (eq major-mode 'inferior-emacs-lisp-mode)
            (--if-let (gethash (current-buffer) nvp-repl--process-buffers)
                (and (buffer-live-p it)
                     (ielm-change-working-buffer it))
              (message "%s not asociated with a source buffer" (current-buffer)))
          (ielm-change-working-buffer (current-buffer)))))

(nvp-repl-add '(sh-mode bash-ts-mode bats-mode)
  :init #'nvp-sh-get-process
  :modes '(shell-mode)
  :procname "shell"
  :bufname "*shell"
  :cd (lambda (&rest _) (funcall-interactively #'sh-cd-here)))

;; return repl for MODE, or default
(defun nvp-repl-for-mode (mode)
  (or (cl-loop for (modes . repl) in nvp-repl-alist
               when (memq mode modes)
               return repl)
      (prog1 nvp-repl-default
        (message
         "%S not explicitely associated with any REPLs: using default shell" mode))))

(defun nvp-repl-ensure (&optional mode)
  "Ensure buffer has a REPL associated with MODE or current `major-mode'."
  (or nvp-repl-current
      (setq nvp-repl-current (nvp-repl-for-mode (or mode major-mode)))))

(eval-when-compile
  ;; may switch storage of REPL vars
  (defmacro repl:val (val)
    (declare (debug t))
    (let ((fn (intern (concat "nvp--repl-" val))))
      `(,fn nvp-repl-current)))

  (defmacro nvp:repl-with (fields &rest body)
    (declare (indent defun) (debug t))
    `(pcase-let (((cl-struct nvp--repl ,@fields) nvp-repl-current))
       ,@body)))

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
  (nvp:repl-with (find-fn)
    (and find-fn (funcall find-fn))))

;; match process buffer
(defun nvp-repl-find-bufname ()
  (nvp:repl-with (bufname proc->buff)
    (when bufname
      (nvp:proc-find bufname
        :key (lambda (p) (buffer-name (funcall proc->buff p)))
        :test #'string-match-p))))

;; match process name
(defun nvp-repl-find-procname ()
  (nvp:repl-with (procname)
    (when procname
      (nvp:proc-find procname :key #'process-name :test #'string-match-p))))

;; match major-mode
(defun nvp-repl-find-modes ()
  (nvp:repl-with (modes proc->buff)
    (when modes
      (nvp:proc-find-if
        (lambda (p-buf) (and p-buf (memq (buffer-local-value 'major-mode p-buf) modes)))
        :key (lambda (p) (funcall proc->buff p))))))

;; -------------------------------------------------------------------
;;; REPL processes

;; non-nil if PROC is alive
(defsubst nvp-repl-live-p (proc) (funcall (nvp--repl-live nvp-repl-current) proc))

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
  (prog1 p-buff (puthash p-buff src-buff nvp-repl--process-buffers)))

;; -------------------------------------------------------------------
;;; Initialize new REPLs

(defun nvp-repl-get-buffer ()
  "Return a REPL buffer if one exists, otherwise attempt to start one."
  (nvp-repl-ensure)
  (or (nvp-repl-buffer)
      (nvp:repl-with (proc->buff buff->proc)
        (when-let ((proc (run-hook-with-args-until-success 'nvp-repl-find-functions))
                   (p-buff (if (processp proc)
                               (funcall proc->buff proc)
                             (prog1 proc
                               (setq proc (funcall buff->proc proc))))))
          (if (nvp-repl-live-p proc)     ; found unregistered live one
              (nvp-repl-update proc (current-buffer) p-buff)
            (remhash p-buff nvp-repl--process-buffers))))
      (or (nvp-repl-start)             ; initialize new REPL
          (user-error "Failed to initialize REPL"))))

(defun nvp-repl-start-callback ()
  "Associate repl with source from repl buffer after it starts."
  (let ((src-buff (current-buffer)))
    `(lambda ()
       (cl-assert (buffer-live-p ,src-buff))
       (if-let ((proc (get-buffer-process (current-buffer)))
                (p-buff (process-buffer proc)))
           (with-current-buffer ,src-buff
             (nvp-repl-update proc ,src-buff p-buff))
         (user-error "Failed to initialize REPL for %S" ,src-buff)))))

(defun nvp-repl-start ()
  "Return a REPL buffer associated with current buffer."
  (if-let (init (repl:val "init-callback"))
      (prog1 'async (funcall init (nvp-repl-start-callback)))
    (let ((wait (repl:val "wait"))
          (proc (funcall (repl:val "init")))
          p-buff)
      (and wait (sit-for wait))
      (unless (processp proc)
        (setq p-buff proc
              proc (funcall (repl:val "buff->proc") p-buff)))
      (cl-assert (processp proc))
      (nvp-repl-update proc (current-buffer) p-buff)
      (repl:val "buff"))))


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
  ;; TODO:
  ;; 1. With some prefix prompt for repl to use
  ;; 2. Set REPLs working directory
  ;; 3. Create new REPL for buffer
  (let ((buff (--if-let (gethash (current-buffer) nvp-repl--process-buffers)
                  (if (buffer-live-p it) it
                    (other-buffer (current-buffer) 'visible))
                (nvp-repl-get-buffer))))
    (unless (eq 'async buff)
      (pop-to-buffer buff nvp-repl--display-action))))

;; -------------------------------------------------------------------
;;; Basic REPL interaction

;; (defun nvp-repl-send-input (&optional no-newline)
;;   "Send repl's current input, appending a final newline unless NO-NEWLINE."
;;   (unless nvp-repl-current
;;     (user-error "No current REPL")))

(defun nvp-repl-send-string (str &optional and-go no-insert no-newline)
  "Send STR to current REPL, appending a final newline unless NO-NEWLINE.
STR is inserted into REPL unless NO-INSERT."
  (unless nvp-repl-current
    (user-error "No REPL associated with buffer."))
  (nvp:repl-with (send-input send-string)
    (unless no-newline (setq str (concat str "\n")))
    (--if-let (nvp-repl-get-buffer)
        (progn
          (if no-insert
              (funcall send-string (nvp-repl-process) str)
            (with-current-buffer it
              (insert str)
              (funcall send-input)))
          (when and-go
            (pop-to-buffer it)))
      (user-error "Couldn't create REPL."))))

(defun nvp-repl-send-region (start end &optional and-go)
  (interactive "r\nP")
  (nvp-repl-send-string (buffer-substring-no-properties start end) and-go))

(defun nvp-repl-send-line (&optional and-go)
  (interactive "P")
  (nvp-repl-send-string
   (buffer-substring-no-properties (nvp:point 'bol) (nvp:point 'eoll))
   and-go))

(defun nvp-repl-send-buffer (&optional and-go)
  (interactive "P")
  (nvp-repl-send-string
   (buffer-substring-no-properties (point-min) (point-max))
   and-go))

(defun nvp-repl-cd-here (&optional dir)
  "Set repl working directory to DIR (default `default-directory').
Prompt with \\[universal-argument]."
  (interactive
   (list (if current-prefix-arg
             (expand-file-name (read-directory-name "Directory: " default-directory))
           default-directory)))
  (when nvp-repl-current
    (--if-let (repl:val "cd")
        (let ((default-directory (or dir default-directory)))
          (funcall it dir)
          (nvp-repl-update (repl:val "proc") (current-buffer)))
      ;; TODO: signal unsupported error
      (user-error "Not supported by current repl."))))

(defun nvp-repl-send-defun (start end &optional and-go)
  (interactive
   (let ((bnds (nvp:tap-or-region 'bdwim 'defun :pulse t)))
     (if bnds (append bnds (list current-prefix-arg))
       (list nil current-prefix-arg))))
  (nvp-repl-send-string
   (buffer-substring-no-properties start end)
   and-go))

;; -------------------------------------------------------------------
;;; Transient 
(require 'transient)

;;;###autoload(autoload 'nvp-repl-menu "nvp-repl")
(transient-define-prefix nvp-repl-menu ()
  "REPL menu"
  [[ :if-non-nil nvp-repl-current
     "Eval"
     ("l" "Line" nvp-repl-send-line)
     ("r" "Region" nvp-repl-send-region)
     ("d" "Defun" nvp-repl-send-defun)
     ("b" "Buffer" nvp-repl-send-buffer)]
   ["Repl"
    ("j" "Jump" nvp-repl-jump)
    ("c" "Change Working directory/buffer" nvp-repl-cd-here
     :if-non-nil nvp-repl-current)]]
  ["Manage Repls"
   (":r" "Remove" nvp-repl-remove)])

(provide 'nvp-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-repl.el ends here
