;;; nvp-repl.el --- Generic Repl functions -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Unified Repl interface:
;;  - `nvp-repl-jump' pops b/w source buffers and repls, starting them when
;;    necessary.
;;
;; TODO:
;; - redirect output
;;
;; Shortcuts: `defslime-repl-shortcut'
;;
;;; Code:

(eval-when-compile (require 'nvp-macro))
(require 'comint)
(require 'nvp)
(nvp:auto "nvp-sh" 'nvp-sh-get-process)
(nvp:decls :p (sh) :f (nvp-repl--init nvp-repl--make-completion-at-point))

(defgroup nvp-repl nil
  "Unified repl."
  :prefix "nvp-repl-"
  :group 'languages)

(defcustom nvp-repl-default 'shell
  "Default Repl when buffer has no repl associations."
  :type 'symbol)

(defcustom nvp-repl-find-functions
  '(nvp-repl-find-custom
    nvp-repl-find-bufname
    nvp-repl-find-procname
    nvp-repl-find-modes)
  "Hook run to find the first applicable Repl process.
Each function takes a process as an argument to test against."
  :type '(repeat symbol))

(defvar-local nvp-repl-load-startup-file t
  "Some repls check this variable before loading a startfile.")

(cl-defstruct (nvp--repl (:constructor nvp-repl-make))
  "Mode specific Repl variables"
  name                               ; repl name
  modes                              ; repl major-modes to consider
  init                               ; init repl => return process
  init-async                         ; repl init is async
  init-use-hook                      ; use temporary repl mode hooks to init repl
  bufname                            ; buffer name to search for
  procname                           ; process name to search for
  find-fn                            ; custom function to find repl
  wait                               ; time to wait for repl
  (live       #'process-live-p)      ; check if repl process is alive
  (process-p  #'processp)            ; check if object is a repl process
  (buf->proc #'get-buffer-process)  ; get buffer associated with process
  (proc->buf #'process-buffer)      ; process associated w/ buffer
  ;; the rest are related to interaction
  filters                                ; filters applied to text sent to repl
  (send-string    #'comint-send-string)  ; function to send string to repl
  (send-input     #'comint-send-input)   ; function to send current input from repl
  (clear-buffer   #'comint-clear-buffer) ; clear repl buffer
  send-line
  send-region
  send-defun
  send-sexp
  send-statement
  send-buffer
  send-file
  ;; Eval: execute silently in Repl, display results in overlay/message
  (eval-output-filter #'nvp-repl--eval-output-filter)
  eval-input-filter
  eval-string
  eval-sexp
  eval-defun
  ;; Repl commands
  commands                        ; repl commands
  (pos-bol #'comint-line-beginning-position)
  cmd-prefix                      ; prefix char for repl commands
  cmd-handlers                    ; handlers for added repl input commands
  help-cmd                        ; command to display Repl help
  cd-cmd                          ; command to change Repl working dir
  pwd-cmd                         ; command to get Repl working directory/buffer
  load-cmd                        ; command to load file/module into repl
  config-cmd                      ; show/set repl config
  ;; History
  history-file
  ;; set internally
  repl-proc                      ; Repl process
  repl-buf)                      ; Repl output buffer (may not have a repl-proc)
(put 'nvp-repl-make 'lisp-indent-function 'defun)


;;; Display

(defcustom nvp-repl-display-action 'other-window
  "Default behaviour of `display-buffer' when popping between REPL/source
buffers."
  :type 'symbol)

(defcustom nvp-repl-dedicated-window nil
  "When non-nil, set repl buffer windows as dedicated."
  :type 'boolean)

(defvar nvp-repl--display-actions
  '((other-window . ((display-buffer--maybe-same-window
                      display-buffer-reuse-window
                      display-buffer-use-some-window
                      display-buffer-pop-up-window)
                     ;; Prefer the largest other window the most ⬎
                     ;; (some-window          . t)
                     ))
    (split-below . ((display-buffer-reuse-window
                     nvp-repl--split-below))))
  "Options for `nvp-repl-display-action'.")

;; FIXME(09/16/24): maybe reuse a window below before splitting
(defun nvp-repl--split-below (buf alist)
  "Split window and display repl below."
  (when-let ((height (cdr (assq 'window-height alist))))
    (when (floatp height)
      (setq height (round (* height (frame-height)))))
    (setq height (- (max height window-min-height)))
    (window--display-buffer buf (split-window-below height) 'window alist)))

(defun nvp-repl--display-action (&optional action)
  (append (assoc-default (or action nvp-repl-display-action)
                         nvp-repl--display-actions)
          `((category             . repl)
            (reusable-frames      . visible)
            (window-height        . 0.4)
            (window-min-height    . 20)
            (inhibit-same-window  . t)
            (inhibit-switch-frame . t)
            ,@(and nvp-repl-dedicated-window
                   `((dedicated . ,nvp-repl-dedicated-window))))))

;;; Caches
;; Cache defined repls
(defvar nvp-repl--repl-cache (make-hash-table))
(defvar nvp-repl-modes '())
;; `major-mode' -> repl mapping
(defvar nvp-repl-cache (make-hash-table))
;; Note: repl buffers may not be processes, eg. slime repls
(defvar nvp-repl--process-buffers (make-hash-table :test #'equal))

(defvar-local nvp-repl-current () "Repl associated with current buffer.")

(defsubst nvp-repl--get-source ()
  (gethash (buffer-name (current-buffer)) nvp-repl--process-buffers))

(defsubst nvp-repl--set-source (repl-buf src-buf)
  (puthash (buffer-name repl-buf) src-buf nvp-repl--process-buffers))

(defun nvp-repl--cleanup-process-buffers ()
  (maphash (lambda (k v)
             (unless (and (get-buffer k)
                          (buffer-live-p v))
               (remhash k nvp-repl--process-buffers)))
           nvp-repl--process-buffers))

;; Get `nvp-repl-current' from either source or Repl buffer
(defun nvp-repl-current ()
  (or nvp-repl-current
      (and-let* ((src (nvp-repl--get-source)))
        (buffer-local-value 'nvp-repl-current src))))

(defun nvp-repl-current-source-buffer ()
  (if nvp-repl-current (current-buffer) (nvp-repl--get-source)))

(defun nvp-repl-current-buffer ()
  (if-let ((nvp-repl-current (nvp-repl-current)))
      (nvp-repl-buffer)
    (user-error "No repl buffer")))

;;;###autoload
(defun nvp-repl-add (mmodes &rest args)
  "Create new mappings of major modes MMODES to repl created from ARGS."
  (unless (listp mmodes) (setq mmodes (cons mmodes nil)))
  (let* ((repl (if (nvp--repl-p (car args)) (car args)
                 (apply #'nvp-repl-make args)))
         (name (nvp--repl-name repl)))
    (unless name
      (user-error "repl name is nil"))
    (puthash name repl nvp-repl--repl-cache)
    (--when-let (nvp--repl-modes repl)
      (setq nvp-repl-modes (cl-remove-duplicates (append it nvp-repl-modes))))
    (dolist (mode mmodes)
      (cl-pushnew name (gethash mode nvp-repl-cache)))))
(put 'nvp-repl-add 'lisp-indent-function 'defun)


;;; Default Shell repl

(nvp:decl sh-cd-here)
(defvar nvp-repl--shell-repl
  (apply #'nvp-repl-make
         (list :name 'shell
               ;; TODO(08/26/24): (comint-terminfo-terminal "xterm-256color")
               :init #'nvp-sh-get-process
               :modes '(shell-mode)
               :procname "shell"
               :bufname "*shell"
               :pwd-cmd "pwd"
               :help-cmd '(:no-arg "help" :with-arg "help %s")
               :load-cmd "source \"%s\""
               :cd-cmd (lambda (dir)
                         (let ((default-directory dir))
                           (funcall-interactively #'sh-cd-here)))))
  "Default repl.")

(nvp-repl-add '(sh-mode bash-ts-mode bats-mode) nvp-repl--shell-repl)


(eval-when-compile
  ;; may switch storage of repl vars
  (defmacro repl:val (val)
    (declare (debug t))
    (let ((fn (intern (concat "nvp--repl-" val))))
      `(,fn nvp-repl-current)))

  (defsubst nvp:repl--choose (repls)
    (if (length> repls 1)
        (intern (completing-read "repl: " repls nil t))
      (car repls))))

(cl-defmacro nvp-with-repl (fields &rest body &key repl allow-repl &allow-other-keys)
  "In a source buffer, do body with bound repl fields.
if allow-repl is non-nil, bind fields from current repl in repl buffers as
well."
  (declare (indent defun)
           (debug (&define cl-macro-list def-form cl-declarations def-body)))
  (nvp:skip-keywords body)
  `(pcase-let (((cl-struct nvp--repl ,@fields)
                (,@(if repl `(or ,repl) '(progn))
                 ,@(if allow-repl '((nvp-repl-current)) '(nvp-repl-current)))))
     ,@body))

(defun nvp-repl-for-mode (mode)
  "Return repl for MODE."
  (--if-let (gethash mode nvp-repl-cache)
      (gethash (nvp:repl--choose it) nvp-repl--repl-cache)
    (prog1 (gethash nvp-repl-default nvp-repl--repl-cache)
      (message
       "%s not explicitely associated with any repls: using default %s"
       mode nvp-repl-default))))

(defun nvp-repl-ensure (&optional mode)
  "Ensure buffer has a repl associated with MODE."
  (or nvp-repl-current
      (setq nvp-repl-current (nvp-repl-for-mode (or mode major-mode)))))


;; -------------------------------------------------------------------
;;; Find Repls

(defun nvp-repl-find-custom ()
  "Find repl using a repls `find-fn'."
  (nvp-with-repl (find-fn)
    (and find-fn (funcall find-fn))))

(defun nvp-repl-find-bufname ()
  "Match process buffer."
  (nvp-with-repl (bufname proc->buf)
    (when bufname
      (nvp:proc-find bufname
        :key (lambda (p) (or (buffer-name (funcall proc->buf p)) regexp-unmatchable))
        :test #'string-match-p))))

(defun nvp-repl-find-procname ()
  "Match process name."
  (nvp-with-repl (procname)
    (when procname
      (nvp:proc-find procname :key #'process-name :test #'string-match-p))))

(defun nvp-repl-find-modes ()
  "Match `major-mode'."
  (nvp-with-repl (modes proc->buf)
    (when modes
      (nvp:proc-find-if
        (lambda (p-buf) (and p-buf (memq (buffer-local-value 'major-mode p-buf) modes)))
        :key (lambda (p) (funcall proc->buf p))))))


;; -------------------------------------------------------------------
;;; Repl Processes

(defsubst nvp-repl-live-p (repl-proc)
  "Return non-nil if REPL-PROC is alive."
  (funcall (nvp--repl-live nvp-repl-current) repl-proc))

;; Check repl has an associated process and it is alive
;; if it had a repl-proc that died, this updates its repl-proc to nil
;; returns the live process when available
(defun nvp-repl-process ()
  "Return repl process or nil."
  (--when-let (repl:val "repl-proc")
    (if (ignore-errors (nvp-repl-live-p it)) it
      (setf (repl:val "repl-proc") nil
            (repl:val "repl-buf") nil))))

(defun nvp-repl-buffer ()
  "Return repl buffer if it has a live process."
  (-some->> (nvp-repl-process)
    (funcall (repl:val "proc->buf"))))

;; Update repls proc/buff and link process-buffer (which may not be an
;; actual process, eg. slime repl) with source buffer
(defun nvp-repl-update (repl-proc src-buf &optional proc-buf)
  "Update current repl's REPL-PROC and SRC-BUF."
  (or proc-buf (setq proc-buf (funcall (repl:val "proc->buf") repl-proc)))
  (setf (repl:val "repl-proc") repl-proc
        (repl:val "repl-buf") proc-buf)
  (prog1 proc-buf (nvp-repl--set-source proc-buf src-buf)))


;; -------------------------------------------------------------------
;;; Minor mode

(defcustom nvp-repl-no-minor-modes
  '(c-mode c-ts-mode c++-mode c++-ts-mode)
  "Modes to not setup minor mode for repl interaction."
  :type '(repeat symbol)
  :group 'nvp-repl)

(nvp:decl nvp-repl-eval-sexp nvp-repl-eval-string
  nvp-repl-pwd nvp-repl-cd nvp-repl-help)

(declare-function nvp-repl-keymap "")
(defvar-keymap nvp-repl-keymap
  :prefix 'nvp-repl-keymap
  "h" #'nvp-repl-help
  "d" #'nvp-repl-cd
  "p" #'nvp-repl-pwd
  "s" #'nvp-repl-set-source
  "q" #'nvp-repl-interrupt-or-kill-process)
;; "l" #'nvp-repl-load-file

(defvar-keymap nvp-repl-minor-mode-map
  :doc "Keymap in repl buffer."
  "C-c C-k" #'nvp-repl-clear
  "C-c C-h" #'nvp-repl-keymap)

(defvar-keymap nvp-repl-source-minor-mode-map
  :doc "Keymap in repl source buffers."
  "C-c C-c"   #'nvp-repl-send-dwim
  "C-c C-s"   #'nvp-repl-send-sexp
  "C-c C-b"   #'nvp-repl-send-buffer
  "C-c C-l"   #'nvp-repl-send-line
  "C-c C-r"   #'nvp-repl-send-region
  "C-c C-f"   #'nvp-repl-send-file
  "C-M-x"     #'nvp-repl-send-defun-or-region
  "C-c C-e"   #'nvp-repl-send-sexp
  "C-c C-x e" #'nvp-repl-eval-sexp
  "C-c C-x s" #'nvp-repl-eval-string
  "C-c C-k"   #'nvp-repl-clear
  "C-c C-q"   #'nvp-repl-interrupt-or-kill-process)

;;;###autoload
(define-minor-mode nvp-repl-minor-mode
  "Minor mode active in Repl buffers."
  :lighter " Ɍepl"
  ;; (when (derived-mode-p 'comint-mode)
  ;;   (add-hook 'comint-output-filter-functions
  ;;             #'comint-postoutput-scroll-to-bottom nil t))
  (when nvp-repl-minor-mode
    (when-let ((nvp-repl-current (nvp-repl-current)))
      (nvp-with-repl (name commands cmd-prefix pos-bol)
        (when commands
          (add-hook
           'completion-at-point-functions
           (nvp-repl--make-completion-at-point name commands cmd-prefix pos-bol)
           nil t))))
    (add-hook 'kill-buffer-hook #'nvp-repl--cleanup-process-buffers nil t)))

;;;###autoload
(define-minor-mode nvp-repl-source-minor-mode
  "Minor mode active in source buffers associated with a Repl."
  :lighter " Ɍepl")

(defun nvp-repl--source-minor-mode-on ()
  (unless (or (memq major-mode
                    (append nvp-repl-no-minor-modes nvp-repl-modes))
              nvp-repl-source-minor-mode)
    (nvp-repl-source-minor-mode 1)))


;; -------------------------------------------------------------------
;;; Starting Repls

;; Runs in a new repl buffer: enable minor mode, setup command handlers,
;; history+hippie, etc.
(defun nvp-repl--setup-repl-buffer (&optional history-file cmd-handlers)
  (nvp-repl-minor-mode)
  (when cmd-handlers
    (nvp-repl-setup-input-filter cmd-handlers))
  (when (and history-file
             ;; XXX(09/24/24): handle for non comint modes?
             (derived-mode-p 'comint-mode) 
             (not (and comint-input-ring-file-name
                       (string= (file-name-base comint-input-ring-file-name)
                                history-file)
                       (memq 'nvp-he-try-expand-history
                             hippie-expand-try-functions-list))))
    (nvp-comint-setup-history history-file)))

;; Enable source minor mode, update source-repl partnership and run repl buffer
;; setup
(defun nvp-repl--init-setup (src-buf repl-proc &optional repl-buf and-go)
  (cl-assert (buffer-live-p src-buf))
  (with-current-buffer src-buf
    (nvp-repl--source-minor-mode-on)
    (let ((buf (nvp-with-repl (history-file cmd-handlers)
                 (with-current-buffer (nvp-repl-update repl-proc src-buf repl-buf)
                   (nvp-repl--setup-repl-buffer history-file cmd-handlers)
                   (current-buffer)))))
      (prog1 buf
        (when and-go
          (pop-to-buffer buf (nvp-repl--display-action)))))))

(defun nvp-repl--make-async-callback (src-buf &optional and-go)
  "Create callback for src-buf that is passed to async repl init.
async repl init functions should call callback with repl-buf and repl-proc."
  `(lambda (repl-proc &optional repl-buf)
     (nvp-repl--init-setup ,src-buf repl-proc repl-buf ,and-go)))

(defun nvp-repl--init-repl-hook (src-buf buf->proc &optional and-go)
  "Associate repl with source from repl mode hook."
  `(lambda ()
     (if-let ((repl-proc (funcall ',buf->proc (current-buffer))))
         (nvp-repl--init-setup ,src-buf repl-proc (current-buffer) ,and-go)
       (user-error "failed to initialize repl for %s" ,src-buf))))

(defun nvp-repl--init-with-repl-hooks (&optional and-go)
  "Run repl initialization via temporary repl mode hooks."
  (nvp-with-repl (init modes buf->proc)
    (let ((hooks (mapcar (lambda (m) (intern (concat (symbol-name m) "-hook"))) modes)))
      (fset 'nvp-repl--init
            `(lambda ()
               (dolist (hook ',hooks) (remove-hook hook #'nvp-repl--init))
               (funcall ,(nvp-repl--init-repl-hook (current-buffer) buf->proc and-go))))
      (condition-case-unless-debug err
          (progn (dolist (hook hooks) (add-hook hook #'nvp-repl--init))
                 (setq prefix-arg current-prefix-arg)
                 (call-interactively init))
        (error (message (error-message-string err))
               (dolist (hook hooks) (remove-hook hook #'nvp-repl--init)))))))

(defun nvp-repl--check-source-buffer (buf &optional prefix)
  "Return source buffer to use.
Check BUF is live and possibly change source depending on PREFIX."
  (or (and buf (null prefix) (buffer-live-p buf) buf)
      (->> (nvp:prompt-with-message "Source buffer: "
             :read-fn #'read-buffer
             :read-args (nil t)
             :message (if prefix "Current buffer %S" "Source buffer %S dead")
             buf)
           (get-buffer)
           (nvp-repl--set-source (current-buffer)))))

(defun nvp-repl-start (&optional prefix repl)
  "Return a Repl buffer associated with current buffer."
  (nvp-with-repl (init init-async init-use-hook wait process-p buf->proc)
    :repl repl
    (cond (init-async (prog1 'async
                        (nvp-repl--make-async-callback (current-buffer) t)))
          (init-use-hook (prog1 'async
                           (nvp-repl--init-with-repl-hooks t)))
          (t (let ((repl-proc (funcall init prefix)) repl-buf)
               (and wait (sit-for wait))
               (unless (funcall process-p repl-proc)
                 (setq repl-buf repl-proc
                       repl-proc (funcall buf->proc repl-buf)))
               (cl-assert (funcall process-p repl-proc))
               (nvp-repl--init-setup (current-buffer) repl-proc repl-buf))))))

(defun nvp-repl-get-buffer (&optional prefix _repl)
  "Return a Repl buffer, creating one when necessary."
  (nvp-repl-ensure)
  (or (and-let* ((buf (nvp-repl-buffer)))
        (prog1 buf (when prefix
                     (nvp-repl--set-source buf (current-buffer))
                     ;; Set working directory to source directory
                     (and (equal '(16) prefix)
                          (nvp-repl-cd)))))
      (nvp-with-repl (process-p proc->buf buf->proc)
        (when-let ((repl-proc (run-hook-with-args-until-success
                               'nvp-repl-find-functions))
                   (proc-buf (if (funcall process-p repl-proc)
                                 (funcall proc->buf repl-proc)
                               (setq repl-proc (funcall buf->proc repl-proc)))))
          (if (nvp-repl-live-p repl-proc)  ; found unregistered live one
              (nvp-repl-update repl-proc (current-buffer) proc-buf)
            (remhash (buffer-name proc-buf) nvp-repl--process-buffers))))
      (nvp-repl-start prefix)           ; initialize new Repl
      (user-error "Failed to initialize Repl")))


;; -------------------------------------------------------------------
;;; Commands

;;;###autoload
(defun nvp-repl-jump (&optional prefix _repl)
  "Jump between source and Repl buffers, staring if necessary.
If the associated source buffer no longer exists, pop to next visible
window.

When call from source buffer, with PREFIX arguments:
 \\[universal-argument]		Set buffer as repl source, passed to init on startup.
 \\[universal-argument] \\[universal-argument] 	Prompt for new repl, becomes current.

When called from a repl buffer with PREFIX:
 \\[universal-argument] Prompt for a buffer to set as source."
  (interactive "P")
  (when (equal '(16) prefix)
    (nvp-repl--cleanup-process-buffers)
    (setq nvp-repl-current nil))
  (let* ((repl-p nil)
         (repl-buf
          (--if-let (nvp-repl--get-source)
              (prog1 (nvp-repl--check-source-buffer it prefix)
                (setq repl-p t))
            (cond ((memq major-mode nvp-repl-modes)
                   (setq repl-p t)
                   (nvp-repl-minor-mode 1)
                   (nvp-repl--check-source-buffer nil prefix))
                  ((--when-let (nvp-repl-get-buffer prefix)
                     (prog1 it (nvp-repl--source-minor-mode-on))))
                  (t nil)))))
    (unless (eq 'async repl-buf)
      (pop-to-buffer
       ;; FIXME(09/23/24): when repl is split below, try to pop to source above
       ;; when it's buried somewhere
       repl-buf (nvp-repl--display-action (and repl-p 'other-window))))))

(defun nvp-repl-remove (mode)
  "Remove any repl associations with MODE."
  (interactive (list (intern (nvp-read-mode))))
  (remhash mode nvp-repl-cache))

;; TODO(5/12/24): Allow starting unassociated REPLs for a buffer?
;;                 Should send commands be disabled?
(defun nvp-repl (repl &optional prefix)
  "Start a Repl for current buffer.
PREFIX is passed to `nvp-repl-jump'."
  (interactive
   (let ((repl (completing-read "Repl: " nvp-repl--repl-cache nil t)))
     (list (gethash repl nvp-repl--repl-cache) current-prefix-arg)))
  (let ((nvp-repl-current repl))
    (funcall-interactively #'nvp-repl-jump prefix repl)))


;; -------------------------------------------------------------------
;;; Repl Interaction
;;
;; TODO: mode specific commands
;; - indirect call for sending region, eg. in perl lines should be joined,
;;   racket/guile remove declarations, etc.
;; - open Repl in project root
;; - sending buffer should call indirect hook to clean input

;; (defun nvp-repl-send-input (&optional no-newline)
;;   "Send repl's current input, appending a final newline unless NO-NEWLINE."
;;   (unless nvp-repl-current
;;     (user-error "No current Repl")))

(defvar-local nvp-repl-eval-input-filter nil
  "When non-nil, apply to input before sending to repl.")

(defun nvp-repl--eval-output-filter (str)
  "Fixup whitespace in STR.
Default output filter for `eval-output-filter'."
  (string-trim
   (cl-reduce
    (lambda (s rep)
      (replace-regexp-in-string (car rep) (cdr rep) s))
    '(("[ \t][ \t]+" . " ")
      ("[\n\r][\n\r]+" . "\n"))
    :initial-value str)))

(defun nvp-repl-send-string (str &optional insert no-newline)
  "Send STR to current Repl.
Unless NO-NEWLINE, append a final newline.
If INSERT, insert STR into REPL (adds to comint history)."
  (unless nvp-repl-current
    (user-error "No Repl associated with buffer."))
  (and nvp-repl-eval-input-filter
       (setq str (funcall nvp-repl-eval-input-filter str)))
  (nvp-with-repl (send-input send-string repl-proc)
    (unless no-newline (setq str (concat str "\n")))
    (--if-let (nvp-repl-get-buffer)
        (progn (if (null insert)
                   (let ((proc (nvp-repl-process)))
                     (with-current-buffer it
                       (insert-before-markers "\n")
                       (funcall send-string proc str)))
                 (with-current-buffer it
                   ;; (goto-char (process-mark repl-proc))
                   (ignore repl-proc)
                   (insert str)
                   (funcall send-input))))
      (user-error "Couldn't create REPL."))))

(defsubst nvp--repl-region (thing)
  (--when-let (bounds-of-thing-at-point thing)
    (list (car it) (cdr it))))

(cl-defmacro nvp--repl-send ( sender sender-args and-go &optional eval
                              &rest fallback
                              &key region eval-fn eval-args
                              &allow-other-keys)
  (declare (indent defun) (debug t))
  (nvp:skip-keywords fallback)
  `(nvp-with-repl
     ,(seq-uniq (delq nil (list (and region 'send-region)
                                (and eval 'eval-input-filter)
                                sender eval-fn)))
     (let ((nvp-repl-eval-input-filter
            ,@(when eval `((and ,eval eval-input-filter)))))
       (cond
        ,@(when (and eval eval-fn)
            `(((and ,eval ,eval-fn) nil)))
        (,sender (funcall-interactively ,sender ,@sender-args))
        (t
         ,(if fallback `(progn ,@fallback)
            (if (not region)
                `(user-error
                  ,(concat "Repl doesnt understand '" (symbol-name sender) "'"))
              (macroexp-let2 nil region
                             (if (symbolp region) `(nvp--repl-region ',region)
                               `(delq nil ,region))
                `(progn
                   ,@(when t             ; (not (eq sender 'send-region))
                       `((and ,region
                              (apply #'nvp-indicate-pulse-region-or-line ,region))))
                   (cond
                    ,@(when (not (eq sender 'send-region))
                        `(((and ,region send-region) (apply send-region ,region))))
                    (t
                     (if ,region
                         (let ((str (apply #'buffer-substring-no-properties
                                           ,region)))
                           ;; ,@(when eval
                           ;;     `((when (and ,eval ,eval-input-filter)
                           ;;         (setq str (funcall ,eval-input-filter str)))))
                           (funcall #'nvp-repl-send-string str))
                       (user-error
                        ,(concat "No region for '" (symbol-name sender) "'"))))))))))))
     ,@(when eval
         `((when ,eval
             (nvp-repl-show-result
              ,@(when eval-fn
                  `((and ,eval-fn
                         (funcall-interactively ,eval-fn ,@eval-args))))
              ;; XXX(08/05/24): refactor insert
              (>= (prefix-numeric-value current-prefix-arg) 64)))))
     (and ,and-go (pop-to-buffer (nvp-repl-buffer)))))

(defsubst nvp-repl--send-parse-prefix (prefix)
  (pcase (prefix-numeric-value prefix)
    (4 (list 'and-go))
    (16 (list nil 'eval))
    (_ nil)))

(defun nvp-repl-send-region (start end &optional and-go eval)
  (interactive
   (progn (unless (region-active-p)
            (user-error "No region"))
          (append (list (region-beginning) (region-end))
                  (nvp-repl--send-parse-prefix current-prefix-arg))))
  (when (repl:val "send-region")
    (nvp-indicate-pulse-region-or-line start end))
  (nvp--repl-send send-region (start end) and-go eval
    :region (list start end)))

(defun nvp-repl-send-line (&optional and-go eval)
  (interactive (nvp-repl--send-parse-prefix current-prefix-arg))
  (nvp--repl-send send-line () and-go eval
    :region (list (nvp:point 'bol) (nvp:point 'eoll))))

(defun nvp-repl-send-sexp (&optional and-go eval)
  (interactive (nvp-repl--send-parse-prefix current-prefix-arg))
  (nvp--repl-send send-sexp () and-go eval
    :eval-fn eval-sexp
    :region sexp))

(defun nvp-repl-send-dwim (&optional and-go eval)
  "Send region, statement, sexp, or line."
  (interactive (nvp-repl--send-parse-prefix current-prefix-arg))
  (cond ((region-active-p)
         (nvp-repl-send-region (region-beginning) (region-end) and-go eval))
        ((repl:val "send-statement")
         (nvp-repl-send-stmt-or-sentence and-go eval))
        ((repl:val "send-sexp")
         (nvp-repl-send-sexp and-go eval))
        (t (nvp-repl-send-line and-go eval))))

(defun nvp-repl--skip-shebang (start)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char start)
      (if (and (eq start (point-min))
               (looking-at-p "#!\\s-*/"))
          (line-beginning-position 2)
        start))))

(defun nvp-repl-send-buffer (&optional and-go)
  (interactive "P")
  (nvp--repl-send send-buffer () and-go nil
    :region (list (nvp-repl--skip-shebang (point-min)) (point-max))))

(defun nvp-repl-send-defun (&optional and-go eval)
  (interactive "P")
  (nvp--repl-send send-defun () and-go eval
    :eval-fn eval-defun
    :region defun))

(defun nvp-repl-send-defun-or-region (&optional and-go eval)
  (interactive (nvp-repl--send-parse-prefix current-prefix-arg))
  (cond ((region-active-p)
         (nvp-repl-send-region (region-beginning) (region-end) and-go eval))
        (t (nvp-repl-send-defun and-go eval))))

(defun nvp-repl-send-stmt-or-sentence (&optional and-go eval)
  (interactive (nvp-repl--send-parse-prefix current-prefix-arg))
  (nvp--repl-send send-statement () and-go eval
    :region (let ((bnds (or (bounds-of-thing-at-point 'statement)
                            (bounds-of-thing-at-point 'sentence))))
              (list (car bnds) (cdr bnds)))))

(defun nvp-repl-send-file (file &optional and-go)
  "Send FILE to repl and pop to repl buffer when AND-GO."
  (interactive
   (let* ((file (buffer-file-name))
          (fname (ignore-errors (file-name-nondirectory file))))
     (list (read-file-name "File: " nil file t fname) current-prefix-arg)))
  (nvp--repl-send send-file (file) and-go nil
    (let ((repl-current nvp-repl-current))
      (with-temp-buffer
        (setq nvp-repl-current repl-current)
        (insert-file-contents-literally file)
        (nvp-repl-send-buffer)))))

(defun nvp-repl-clear ()
  "Clear repl output buffer."
  (interactive)
  (if-let* ((repl-buf (nvp-repl-current-buffer)))
      (nvp-with-repl (clear-buffer) :allow-repl t
        (if clear-buffer
            (with-current-buffer repl-buf (funcall clear-buffer))
          (user-error "unimplemented")))
    (user-error "No current repl")))

(defun nvp-repl-set-source ()
  "Set source buffer for repl."
  (interactive)
  (nvp-repl--check-source-buffer nil))

(defun nvp-repl-interrupt-or-kill-process (kill)
  "Interrupt repl process or KILL with prefix."
  (interactive "P")
  (when-let ((proc (ignore-errors (nvp-repl-process))))
    (if kill
        (kill-process proc)
      (interrupt-process proc))))

(provide 'nvp-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-repl.el ends here
