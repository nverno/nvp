;;; nvp-repl.el --- Generic REPL functions -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Unified REPL interface:
;;  - `nvp-repl-jump' pops b/w source buffers and repls, starting them when
;;    necessary.
;; 
;; TODO:
;; - redirect output
;; - eval + print results
;; - command to interrupt / kill repl process
;;; Code:

(eval-when-compile
  (require 'nvp-macro)
  (require 'comint))
(require 'nvp)
(nvp:auto "nvp-sh" 'nvp-sh-get-process)
(nvp:decls :p (hippie comint sh ielm) :f (nvp-repl--init))

(cl-defstruct (nvp--repl (:constructor nvp-repl-make))
  "Mode specific REPL variables"
  name                               ; REPL name
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
  filters                                ; filters applied to text sent to REPL
  (send-string    #'comint-send-string)  ; function to send string to REPL
  (send-input     #'comint-send-input)   ; function to send current input from REPL
  send-line
  send-region
  send-defun
  send-sexp
  send-buffer
  send-file
  ;; Eval: execute silently in REPL, return output
  eval-string
  eval-sexp
  ;; REPL commands
  help-cmd                        ; command to display REPL help
  cd-cmd                          ; command to change REPL working dir
  pwd-cmd                         ; command to get REPL working directory/buffer
  ;; History
  history-file
  ;; set internally
  proc                               ; REPL process
  buff                               ; REPL output buffer (may not have a proc)
  )
(put 'nvp-repl-make 'lisp-indent-function 'defun)

;; Cache defined repls
(defvar nvp-repl--repl-cache (make-hash-table))
;; `major-mode' -> repl mapping
(defvar nvp-repl-cache (make-hash-table))
;; Note: repl buffers may not be processes, eg. slime repls
(defvar nvp-repl--process-buffers (make-hash-table))

(defvar-local nvp-repl-current ()
  "REPL associated with current buffer.")

;;;###autoload
(defun nvp-repl-add (mmodes &rest args)
  "Create new mappings of major modes MMODES to repl created from ARGS."
  (unless (listp mmodes) (setq mmodes (cons mmodes nil)))
  (let* ((repl (if (nvp--repl-p (car args)) (car args)
                 (apply #'nvp-repl-make args)))
         (name (nvp--repl-name repl)))
    (unless name
      (user-error "Repl name is nil"))
    (puthash name repl nvp-repl--repl-cache)
    (dolist (mode mmodes)
      (cl-pushnew name (gethash mode nvp-repl-cache)))))
(put 'nvp-repl-add 'lisp-indent-function 'defun)

;; default REPL to use - shell
(nvp:decl sh-cd-here)
(defvar nvp-repl--shell-repl
  (apply #'nvp-repl-make
         (list :name 'shell
               :init #'nvp-sh-get-process
               :modes '(shell-mode)
               :procname "shell"
               :bufname "*shell"
               :pwd-cmd "pwd"
               :help-cmd (lambda (&optional thing)
                           (nvp-repl-send-string
                            (if thing (format "help %s" thing) "help")))
               :cd-cmd (lambda (dir) (let ((default-directory dir))
                                  (funcall-interactively #'sh-cd-here)))
               :history-file ".bash_history")))

(defvar nvp-repl-default 'shell
  "Default REPL when buffer has no repl associations.")

;;; Initialize some REPLs
(nvp-repl-add '(sh-mode bash-ts-mode bats-mode) nvp-repl--shell-repl)

(nvp:decl ielm-send-input)
(nvp-repl-add '(emacs-lisp-mode lisp-interaction-mode)
  :name 'ielm
  :init #'ielm
  :modes '(inferior-emacs-lisp-mode)
  :procname "ielm"
  :bufname "*ielm"
  :send-input #'ielm-send-input
  :eval-sexp #'eval-last-sexp
  :wait 0.1
  :history-file ".ielm_history"
  :help-cmd "(describe-mode ielm-working-buffer)"
  :pwd-cmd #'ielm-print-working-buffer
  :cd-cmd (lambda (&rest _)
            (if (eq major-mode 'inferior-emacs-lisp-mode)
                (--if-let (gethash (current-buffer) nvp-repl--process-buffers)
                    (and (buffer-live-p it)
                         (ielm-change-working-buffer it))
                  (message "%s not asociated with a source buffer" (current-buffer)))
              (ielm-change-working-buffer (current-buffer)))))

(eval-when-compile
  ;; may switch storage of REPL vars
  (defmacro repl:val (val)
    (declare (debug t))
    (let ((fn (intern (concat "nvp--repl-" val))))
      `(,fn nvp-repl-current)))

  (defmacro nvp:repl-with (fields &rest body)
    (declare (indent defun) (debug t))
    `(pcase-let (((cl-struct nvp--repl ,@fields) nvp-repl-current))
       ,@body))

  (defsubst nvp:repl--choose (repls)
    (if (length> repls 1)
        (intern (completing-read "Repl: " repls nil t))
      (car repls))))

;; return repl for MODE, or default
(defun nvp-repl-for-mode (mode)
  (--if-let (gethash mode nvp-repl-cache)
      (gethash (nvp:repl--choose it) nvp-repl--repl-cache)
    (prog1 (gethash nvp-repl-default nvp-repl--repl-cache)
      (message
       "%S not explicitely associated with any REPLs: using default %S"
       mode nvp-repl-default))))

(defun nvp-repl-ensure (&optional mode)
  "Ensure buffer has a REPL associated with MODE or current `major-mode'."
  (or nvp-repl-current
      (setq nvp-repl-current (nvp-repl-for-mode (or mode major-mode)))))

;; -------------------------------------------------------------------
;;; Functions to find REPLs

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

(defun nvp-repl-get-buffer (&optional prefix)
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
      (or (--when-let (nvp-repl-start prefix)  ; initialize new REPL
            (nvp-repl-minor-mode 1)
            it)
          (user-error "Failed to initialize REPL"))))

(defun nvp-repl-start-callback (&optional and-go)
  "Associate repl with source from repl buffer after it starts."
  (let ((src-buff (current-buffer)))
    `(lambda ()
       (cl-assert (buffer-live-p ,src-buff))
       (if-let ((proc (get-buffer-process (current-buffer)))
                (p-buff (process-buffer proc)))
           (progn
             (with-current-buffer ,src-buff
               (nvp-repl-update proc ,src-buff p-buff))
             ,(when and-go '(pop-to-buffer (current-buffer))))
         (user-error "Failed to initialize REPL for %S" ,src-buff)))))

(defun nvp--repl-init-with-callback ()
  "Run repl initialization with temporary repl mode hooks added to associate
repl with source buffer."
  (nvp:repl-with (init-callback modes)
    (let ((hooks (mapcar (lambda (m) (intern (concat (symbol-name m) "-hook"))) modes)))
      (fset 'nvp-repl--init
            `(lambda ()
               (dolist (hook ',hooks) (remove-hook hook #'nvp-repl--init))
               (funcall ,(nvp-repl-start-callback t))))
      (condition-case nil
          (progn (dolist (hook hooks) (add-hook hook #'nvp-repl--init))
                 (call-interactively init-callback))
        (error (dolist (hook hooks) (remove-hook hook #'nvp-repl--init)))))))

(defun nvp-repl--setup-history (proc-buff)
  (nvp:repl-with (history-file)
    (when history-file
      (with-current-buffer proc-buff
        ;; XXX: handle non-comint modes
        (when (and (derived-mode-p 'comint-mode)
                   (not (and comint-input-ring-file-name
                             (string= (file-name-base comint-input-ring-file-name)
                                      history-file)
                             (memq 'nvp-he-try-expand-history
                                   hippie-expand-try-functions-list))))
          (nvp-comint-setup-history history-file))))))

(defun nvp-repl-start (&optional prefix)
  "Return a REPL buffer associated with current buffer."
  (if (repl:val "init-callback")
      (prog1 'async (nvp--repl-init-with-callback))
    (nvp:repl-with (wait init buff->proc proc->buff)
      (let ((proc (funcall init prefix)) p-buff)
        (and wait (sit-for wait))
        (unless (processp proc)
          (setq p-buff proc
                proc (funcall buff->proc p-buff)))
        (cl-assert (processp proc))
        (nvp-repl-update proc (current-buffer) p-buff)
        (nvp-repl--setup-history (funcall proc->buff proc))
        (repl:val "buff")))))

(defun nvp-repl--check-source-buffer (buf &optional prefix)
  (if (and (null prefix) (buffer-live-p buf)) buf
    (let ((src-buf
           (nvp:prompt-with-message "Source buffer: "
             :read-fn #'read-buffer
             :read-args (nil t)
             :message (if prefix "Current buffer %S" "Source buffer %S dead") buf)))
      (puthash (current-buffer) (get-buffer src-buf) nvp-repl--process-buffers))))


;; -------------------------------------------------------------------
;;; Commands

;; `display-buffer' action for popping between REPL/source buffers
(defvar nvp-repl--display-action
  '((display-buffer-reuse-window
     display-buffer-use-some-window
     display-buffer-pop-up-window)
    (reusable-frames      . visible)
    (inhibit-switch-frame . t)
    (inhibit-same-window  . t)))

;;;###autoload
(defun nvp-repl-jump (&optional prefix)
  "Jump between source and REPL buffers, staring if necessary.
If the associated source buffer no longer exists, pop to next visible
window.

When call from source buffer, with PREFIX arguments:
 \\[universal-argument]		Prefix passed to repl init function on startup.
 \\[universal-argument] \\[universal-argument] 	Prompt for new repl, becomes current.

When called from a repl buffer with PREFIX:
 \\[universal-argument] Prompt for a buffer to set as source."
  (interactive "P")
  ;; TODO: force create new REPL for buffer
  (when (equal '(16) prefix) (setq nvp-repl-current nil))
  (let ((buff (--if-let (gethash (current-buffer) nvp-repl--process-buffers)
                  (nvp-repl--check-source-buffer it prefix)
                (nvp-repl-get-buffer prefix))))
    (unless (eq 'async buff)
      (pop-to-buffer buff nvp-repl--display-action))))

(defun nvp-repl-remove (mode)
  "Remove any repl associations with MODE."
  (interactive (list (intern (nvp-read-mode))))
  (remhash mode nvp-repl-cache))

;; -------------------------------------------------------------------
;;; Basic REPL interaction
;;
;; TODO: mode specific commands
;; - indirect call for sending region, eg. in perl lines should be joined,
;;   racket/guile remove declarations, etc.
;; - open REPL in project root
;; - sending buffer should call indirect hook to clean input

;; (defun nvp-repl-send-input (&optional no-newline)
;;   "Send repl's current input, appending a final newline unless NO-NEWLINE."
;;   (unless nvp-repl-current
;;     (user-error "No current REPL")))

(defun nvp-repl-send-string (str &optional no-insert no-newline)
  "Send STR to current REPL, appending a final newline unless NO-NEWLINE.
STR is inserted into REPL unless NO-INSERT."
  (unless nvp-repl-current
    (user-error "No REPL associated with buffer."))
  (nvp:repl-with (send-input send-string proc)
    (unless no-newline (setq str (concat str "\n")))
    (--if-let (nvp-repl-get-buffer)
        (progn (if no-insert
                   (funcall send-string (nvp-repl-process) str)
                 (with-current-buffer it
                   (goto-char (process-mark proc))
                   (insert str)
                   (funcall send-input))))
      (user-error "Couldn't create REPL."))))

(eval-when-compile
  (defmacro nvp:region (thing)
    `(--when-let (bounds-of-thing-at-point ',thing)
       (list (car it) (cdr it))))
  
  (cl-defmacro nvp:repl-send ( sender sender-args and-go
                               &rest fallback &key region &allow-other-keys)
    (declare (indent defun))
    (nvp:skip-keywords fallback)
    `(nvp:repl-with ,(if region (seq-uniq (list 'send-region sender))
                       (list sender))
       (if ,sender (funcall-interactively ,sender ,@sender-args)
         ,(if fallback `((,@fallback))
            (if (not region)
                `(user-error
                  ,(concat "Repl doesnt understand '" (symbol-name sender) "'"))
              (macroexp-let2 nil region
                             (if (symbolp region) `(nvp:region ,region)
                               `(delq nil ,region))
                `(progn
                   ,@(when (not (eq sender 'send-region))
                       `((and ,region
                              (apply #'nvp-indicate-pulse-region-or-line ,region))))
                   (cond
                    ,@(when (not (eq sender 'send-region))
                        `(((and ,region send-region) (apply send-region ,region))))
                    (t
                     (if ,region
                         (funcall #'nvp-repl-send-string
                                  (apply #'buffer-substring-no-properties ,region))
                       (user-error
                        ,(concat "No region for '" (symbol-name sender) "'"))))))))))
       (and ,and-go (pop-to-buffer (nvp-repl-buffer))))))

(defun nvp-repl-send-region (start end &optional and-go)
  (interactive "r\nP")
  (when (repl:val "send-region")
    (nvp-indicate-pulse-region-or-line start end))
  (nvp:repl-send send-region (start end) and-go :region (list start end)))

(defun nvp-repl-send-line (&optional and-go)
  (interactive "P")
  (nvp:repl-send send-line () and-go
    :region (list (nvp:point 'bol) (nvp:point 'eoll))))

(defun nvp-repl-send-sexp (&optional and-go)
  (interactive "P")
  (nvp:repl-send send-sexp () and-go :region sexp))

(defun nvp-repl-send-dwim (&optional and-go)
  "Send region or sexp (if defined by repl), or fallback to line."
  (interactive "P")
  (cond
   ((region-active-p)
    (nvp-repl-send-region (region-beginning) (region-end) and-go))
   ((repl:val "send-sexp")
    (nvp-repl-send-sexp and-go))
   (t (nvp-repl-send-line and-go))))

(defun nvp-repl-send-buffer (&optional and-go)
  (interactive "P")
  (nvp:repl-send send-buffer () and-go :region buffer))

(defun nvp-repl-send-defun (&optional and-go)
  (interactive "P")
  (nvp:repl-send send-defun () and-go :region defun))

(defun nvp-repl-send-defun-or-region (&optional and-go)
  (interactive "P")
  (cond
   ((region-active-p)
    (nvp-repl-send-region (region-beginning) (region-end) and-go))
   (t (nvp-repl-send-defun and-go))))

(defun nvp-repl-send-file (file &optional and-go)
  (interactive
   (let* ((file (buffer-file-name))
          (fname (ignore-errors (file-name-nondirectory file))))
     (list (read-file-name "File: " nil file t fname) current-prefix-arg)))
  (nvp:repl-send send-file (file) and-go))

;; -------------------------------------------------------------------
;;; Eval

(defun nvp-repl-eval-string (&optional insert)
  (interactive "P")
  (nvp:repl-with (eval-string)
    (unless eval-string
      (user-error "unsupported: eval-string"))
    (let ((res (funcall-interactively eval-string)))
      (when (and res (not (string-empty-p res)))
        (if insert
            (let ((standard-output (current-buffer)))
              (princ res))
          (message "%S" res))))))

(defun nvp-repl-eval-sexp (&optional insert)
  (interactive "P")
  (nvp:repl-with (eval-sexp)
    (unless eval-sexp
      (user-error "unsupported: eval-sexp"))
    (funcall-interactively eval-sexp insert)
    ;; (if insert
    ;;     (let ((standard-output (current-buffer)))
    ;;       (princ res))
    ;;   (message "%S" res))
    ))

;; -------------------------------------------------------------------
;;; REPL commands

(defun nvp-repl-current ()
  (or nvp-repl-current
      (--when-let (gethash (current-buffer) nvp-repl--process-buffers)
        (buffer-local-value 'nvp-repl-current it))))

(eval-when-compile
  (defsubst nvp:repl-src-buffer ()
    (if nvp-repl-current (current-buffer)
      (gethash (current-buffer) nvp-repl--process-buffers)))

  (defmacro nvp:with-current-repl (&rest body)
    `(if-let ((nvp-repl-current (nvp-repl-current)))
         (progn ,@body)
       (user-error "No current repl.")))

  (defmacro nvp:with-repl-vals (vals &rest body)
    (declare (indent 1))
    `(nvp:with-current-repl
      (pcase-let (((cl-struct nvp--repl ,@vals) nvp-repl-current))
        ,@body)))

  (defmacro nvp:call-repl-cmd (cmd &optional args &rest body)
    (declare (indent 2))
    `(nvp:with-repl-vals (,cmd)
       (if (null ,cmd) (user-error "not implemented")
         (progn (pcase ,cmd
                  ((pred stringp)
                   (nvp-repl-send-string
                    ,(if args `(format ,cmd ,@args) `,cmd)))
                  ((pred functionp) (funcall ,cmd ,@args))
                  ((pred symbolp) (eval ,cmd))
                  ,@(when args
                      `((`(:no-arg ,no-arg :with-arg ,with-arg)
                         (cl-assert (and (stringp no-arg) (stringp with-arg)))
                         (nvp-repl-send-string
                          (if ,@args (format with-arg ,@args) no-arg)))))
                  (_ (user-error
                      ,(concat "unhandled " (symbol-name cmd) " type: '%S'") ,cmd)))
                ,@body))))
  
  (defmacro nvp:with-repl-src-buffer (&rest body)
    (declare (indent defun) (debug t))
    (nvp:with-syms (buf)
      `(let ((,buf (nvp:repl-src-buffer)))
         (unless (and ,buf (buffer-live-p ,buf))
           (user-error "No source buffer associated with current buffer."))
         (with-current-buffer ,buf
           ,@body)))))

(defun nvp-repl-cd (&optional dir)
  "Set repl working directory to DIR (default `default-directory').
Prompt with \\[universal-argument]."
  (interactive
   (list (if current-prefix-arg
             (expand-file-name (read-directory-name "Directory: " default-directory))
           default-directory)))
  (unless dir (setq dir default-directory))
  (nvp:with-repl-src-buffer
    (let ((default-directory dir))
      (nvp:call-repl-cmd cd-cmd (default-directory)
        (nvp-repl-update (repl:val "proc") (current-buffer))
        (with-current-buffer (nvp-repl-buffer)
          (setq default-directory dir))))))

(defun nvp-repl-pwd (&optional and-go)
  "Print repl current working directory/buffer."
  (interactive "P")
  (nvp:call-repl-cmd pwd-cmd nil
    (and and-go (pop-to-buffer (nvp-repl-buffer)))))

(defun nvp-repl-help (&optional thing and-go)
  "Print repl help for THING or repl."
  (interactive
   (if current-prefix-arg (list (read-string "Help: " (thing-at-point 'symbol)) t)))
  (nvp:call-repl-cmd help-cmd (thing)
    (and and-go (pop-to-buffer (nvp-repl-buffer)))))

;; -------------------------------------------------------------------
;;; Minor mode

(defvar-keymap nvp-repl-minor-mode-map
  "C-c C-c"   #'nvp-repl-send-dwim
  "C-c C-e"   #'nvp-repl-send-sexp
  "C-c C-x e" #'nvp-repl-eval-sexp
  "C-c C-b"   #'nvp-repl-send-buffer
  "C-c C-l"   #'nvp-repl-send-line
  "C-c C-r"   #'nvp-repl-send-region
  "C-c C-f"   #'nvp-repl-send-file
  "C-M-x"     #'nvp-repl-send-defun-or-region)

;;;###autoload
(define-minor-mode nvp-repl-minor-mode
  "REPL Minor mode."
  :lighter " Ɍepl")

;; -------------------------------------------------------------------
;;; Transient 

(require 'transient)

;;;###autoload(autoload 'nvp-repl-menu "nvp-repl")
(transient-define-prefix nvp-repl-menu ()
  "REPL menu"
  [[ :if-non-nil nvp-repl-current
     "Send"
     ("s" "Last sexp" nvp-repl-send-sexp)
     ("l" "Line or region" nvp-repl-send-line)
     ("r" "Region" nvp-repl-send-region)
     ("f" "Defun" nvp-repl-send-defun)
     ("d" "Defun or region" nvp-repl-send-defun-or-region)
     ("b" "Buffer" nvp-repl-send-buffer)
     ("F" "Load File" nvp-repl-send-file)]
   [ :if-non-nil nvp-repl-current
     "Eval"
     ("e" "Last sexp" nvp-repl-eval-sexp)]
   [ :if nvp-repl-current
     "Commands"
     ("h" "Help" nvp-repl-help :transient t)
     ("w" "Show working directory/buffer" nvp-repl-pwd :transient t)
     ("W" "Change Working directory/buffer" nvp-repl-cd)]]
  [["Repl"
    ("j" "Jump" nvp-repl-jump)]
   ["Manage Repls"
    (":r" "Remove" nvp-repl-remove)]])

(provide 'nvp-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-repl.el ends here
