;;; nvp-repl.el --- Generic REPL functions -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Unified REPL interface:
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
  "Default REPL when buffer has no repl associations."
  :type 'symbol)

(defcustom nvp-repl-find-functions
  '(nvp-repl-find-custom
    nvp-repl-find-bufname
    nvp-repl-find-procname
    nvp-repl-find-modes)
  "Hook run to find the first applicable REPL process.
Each function takes a process as an argument to test against."
  :type '(repeat symbol))

(defvar-local nvp-repl-load-startup-file t
  "Some repls check this variable before loading a startfile.")

(cl-defstruct (nvp--repl (:constructor nvp-repl-make))
  "Mode specific REPL variables"
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
  (buff->proc #'get-buffer-process)  ; get buffer associated with process
  (proc->buff #'process-buffer)      ; process associated w/ buffer
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
  ;; Eval: execute silently in REPL, return output
  eval-filter
  eval-string
  eval-sexp
  ;; REPL commands
  commands                        ; repl commands
  (pos-bol #'comint-line-beginning-position)
  cmd-prefix                      ; prefix char for repl commands
  help-cmd                        ; command to display REPL help
  cd-cmd                          ; command to change REPL working dir
  pwd-cmd                         ; command to get REPL working directory/buffer
  ;; History
  history-file
  ;; set internally
  repl-proc                      ; REPL process
  repl-buff)                     ; REPL output buffer (may not have a repl-proc)
(put 'nvp-repl-make 'lisp-indent-function 'defun)


;;; Display

(defcustom nvp-repl-display-action 'other-window
  "Default behaviour of `display-buffer' when popping between REPL/source
buffers."
  :type 'symbol)

(defvar nvp-repl--display-actions
  '((other-window
     . ((display-buffer-reuse-window
         display-buffer-use-some-window
         display-buffer-pop-up-window)
        (reusable-frames      . visible)
        (inhibit-switch-frame . t)
        (inhibit-same-window  . t)))
    (split-below
     . ((display-buffer-reuse-window
         nvp-repl--split-below)
        (window-height        . 0.4)
        (inhibit-same-window  . t))))
  "Options for `nvp-repl-display-action'.")

;;; FIXME: if there is already a window below, should try to use that instead of
;;; splitting
(defun nvp-repl--split-below (buf alist)
  "Split window and display repl below."
  (when-let ((height (cdr (assq 'window-height alist))))
    (when (floatp height)
      (setq height (round (* height (frame-height)))))
    (setq height (- (max height window-min-height)))
    (window--display-buffer buf (split-window-below height) 'window alist)))

(defun nvp-repl--display-action (&optional action)
  (assoc-default (or action nvp-repl-display-action) nvp-repl--display-actions))

;;; Caches
;; Cache defined repls
(defvar nvp-repl--repl-cache (make-hash-table))
(defvar nvp-repl-modes '())
;; `major-mode' -> repl mapping
(defvar nvp-repl-cache (make-hash-table))
;; Note: repl buffers may not be processes, eg. slime repls
(defvar nvp-repl--process-buffers (make-hash-table :test #'equal))

(defvar-local nvp-repl-current () "REPL associated with current buffer.")

(defsubst nvp-repl--get-source ()
  (declare (pure t) (side-effect-free t))
  (gethash (current-buffer) nvp-repl--process-buffers))

(defsubst nvp-repl--set-source (repl-buf src-buf)
  (puthash repl-buf src-buf nvp-repl--process-buffers))

(defun nvp-repl--cleanup-process-buffers ()
  (maphash (lambda (k v)
             (unless (and (buffer-live-p k)
                          (buffer-live-p v))
               (remhash k nvp-repl--process-buffers)))
           nvp-repl--process-buffers))

;; Get `nvp-repl-current' from either source or REPL buffer
(defun nvp-repl-current ()
  (or nvp-repl-current
      (--when-let (nvp-repl--get-source)
        (buffer-local-value 'nvp-repl-current it))))

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

;; default repl to use - shell
(nvp:decl sh-cd-here)
(defvar nvp-repl--shell-repl
  (apply #'nvp-repl-make
         (list :name 'shell
               :init #'nvp-sh-get-process
               :modes '(shell-mode)
               :procname "shell"
               :bufname "*shell"
               :pwd-cmd "pwd"
               :help-cmd '(:no-arg "help" :with-arg "help %s")
               :cd-cmd (lambda (dir)
                         (let ((default-directory dir))
                           (funcall-interactively #'sh-cd-here)))
               :history-file ".bash_history")))

;;; initialize some repls
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

(cl-defmacro nvp-with-repl (fields &rest body &key allow-repl &allow-other-keys)
  "in a source buffer, do body with bound repl fields.
if allow-repl is non-nil, bind fields from current repl in repl buffers as
well."
  (declare (indent defun)
           (debug (&define cl-macro-list def-form cl-declarations def-body)))
  (nvp:skip-keywords body)
  `(pcase-let (((cl-struct nvp--repl ,@fields)
                ,(if allow-repl '(nvp-repl-current) 'nvp-repl-current)))
     ,@body))

;; return repl for mode, or default
(defun nvp-repl-for-mode (mode)
  (--if-let (gethash mode nvp-repl-cache)
      (gethash (nvp:repl--choose it) nvp-repl--repl-cache)
    (prog1 (gethash nvp-repl-default nvp-repl--repl-cache)
      (message
       "%s not explicitely associated with any repls: using default %s"
       mode nvp-repl-default))))

(defun nvp-repl-ensure (&optional mode)
  "ensure buffer has a repl associated with mode or current `major-mode'."
  (or nvp-repl-current
      (setq nvp-repl-current (nvp-repl-for-mode (or mode major-mode)))))

;; -------------------------------------------------------------------
;;; functions to find repls

;; find repl using a custom function
(defun nvp-repl-find-custom ()
  (nvp-with-repl (find-fn)
    (and find-fn (funcall find-fn))))

;; match process buffer
(defun nvp-repl-find-bufname ()
  (nvp-with-repl (bufname proc->buff)
    (when bufname
      (nvp:proc-find bufname
        :key (lambda (p) (or (buffer-name (funcall proc->buff p)) regexp-unmatchable))
        :test #'string-match-p))))

;; match process name
(defun nvp-repl-find-procname ()
  (nvp-with-repl (procname)
    (when procname
      (nvp:proc-find procname :key #'process-name :test #'string-match-p))))

;; match major-mode
(defun nvp-repl-find-modes ()
  (nvp-with-repl (modes proc->buff)
    (when modes
      (nvp:proc-find-if
        (lambda (p-buf) (and p-buf (memq (buffer-local-value 'major-mode p-buf) modes)))
        :key (lambda (p) (funcall proc->buff p))))))

;; -------------------------------------------------------------------
;;; repl processes

;; non-nil if repl-proc is alive
(defsubst nvp-repl-live-p (repl-proc) (funcall (nvp--repl-live nvp-repl-current) repl-proc))

;; check repl has an associated process and it is alive
;; if it had a repl-proc that died, this updates its repl-proc to nil
;; returns the live process when available
(defun nvp-repl-process ()
  (--when-let (repl:val "repl-proc")
    (if (ignore-errors (nvp-repl-live-p it)) it
      (setf (repl:val "repl-proc") nil
            (repl:val "repl-buff") nil))))

;; get repl buffer if it has a live process
(defun nvp-repl-buffer ()
  (-some->> (nvp-repl-process)
    (funcall (repl:val "proc->buff"))))

;; update repls proc/buff and link process-buffer (which may not be an
;; actual process, eg. slime repl) with source buffer
(defun nvp-repl-update (repl-proc src-buff &optional p-buff)
  (nvp:defq p-buff (funcall (repl:val "proc->buff") repl-proc))
  (setf (repl:val "repl-proc") repl-proc
        (repl:val "repl-buff") p-buff)
  (prog1 p-buff (nvp-repl--set-source p-buff src-buff)))

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
  "C-c C-e"   #'nvp-repl-send-sexp
  "C-c C-b"   #'nvp-repl-send-buffer
  "C-c C-l"   #'nvp-repl-send-line
  "C-c C-r"   #'nvp-repl-send-region
  "C-c C-f"   #'nvp-repl-send-file
  "C-M-x"     #'nvp-repl-send-defun-or-region
  "C-c C-x e" #'nvp-repl-eval-sexp
  "C-c C-x s" #'nvp-repl-eval-string
  "C-c C-k"   #'nvp-repl-clear
  "C-c C-q"   #'nvp-repl-interrupt-or-kill-process)

;;;###autoload
(define-minor-mode nvp-repl-minor-mode
  "REPL buffer minor mode."
  :lighter " Ɍepl"
  ;; (when (derived-mode-p 'comint-mode)
  ;;   (add-hook 'comint-output-filter-functions
  ;;             #'comint-postoutput-scroll-to-bottom nil t))
  (when-let ((nvp-repl-current (nvp-repl-current)))
    (nvp-with-repl (name commands cmd-prefix pos-bol)
      (when commands
        (add-hook
         'completion-at-point-functions
         (nvp-repl--make-completion-at-point name commands cmd-prefix pos-bol)
         nil t)))))

;;;###autoload
(define-minor-mode nvp-repl-source-minor-mode
  "REPL source buffer minor mode."
  :lighter " Ɍepl")

(defun nvp-repl--source-minor-mode-on ()
  (unless (or (memq major-mode (append nvp-repl-no-minor-modes nvp-repl-modes))
              nvp-repl-source-minor-mode)
    (nvp-repl-source-minor-mode 1)))

;; -------------------------------------------------------------------
;;; Initialize new repls

(defun nvp-repl--setup-repl-buffer (&optional history-file)
  (nvp-repl-minor-mode)
  (when (and history-file
             (derived-mode-p 'comint-mode) ; xxx: handle for non comint modes?
             (not (and comint-input-ring-file-name
                       (string= (file-name-base comint-input-ring-file-name)
                                history-file)
                       (memq 'nvp-he-try-expand-history
                             hippie-expand-try-functions-list))))
    (nvp-comint-setup-history history-file)))

(defun nvp-repl--init-setup (src-buf repl-proc &optional repl-buf and-go)
  (cl-assert (buffer-live-p src-buf))
  (with-current-buffer src-buf
    (nvp-repl--source-minor-mode-on)
    (let ((buf (nvp-with-repl (history-file)
                 (with-current-buffer (nvp-repl-update repl-proc src-buf repl-buf)
                   (nvp-repl--setup-repl-buffer history-file)
                   (current-buffer)))))
      (when and-go
        (pop-to-buffer buf (nvp-repl--display-action))))))

(defun nvp-repl--make-async-callback (src-buf &optional and-go)
  "create callback for src-buf that is passed to async repl init.
async repl init functions should call callback with repl-buf and repl-proc."
  `(lambda (repl-proc &optional repl-buf)
     (nvp-repl--init-setup ,src-buf repl-proc repl-buf ,and-go)))

(defun nvp-repl--init-repl-hook (src-buf buff->proc &optional and-go)
  "associate repl with source from repl mode hook."
  `(lambda ()
     (if-let ((repl-proc (funcall ',buff->proc (current-buffer))))
         (nvp-repl--init-setup ,src-buf repl-proc (current-buffer) ,and-go)
       (user-error "failed to initialize repl for %s" ,src-buf))))

(defun nvp-repl--init-with-repl-hooks (&optional and-go)
  "run repl initialization with temporary repl mode hooks added to associate
repl with source buffer."
  (nvp-with-repl (init modes buff->proc)
    (let ((hooks (mapcar (lambda (m) (intern (concat (symbol-name m) "-hook"))) modes)))
      (fset 'nvp-repl--init
            `(lambda ()
               (dolist (hook ',hooks) (remove-hook hook #'nvp-repl--init))
               (funcall ,(nvp-repl--init-repl-hook (current-buffer) buff->proc and-go))))
      (condition-case-unless-debug err
          (progn (dolist (hook hooks) (add-hook hook #'nvp-repl--init))
                 (setq prefix-arg current-prefix-arg)
                 (call-interactively init))
        (error (message (error-message-string err))
               (dolist (hook hooks) (remove-hook hook #'nvp-repl--init)))))))

(defun nvp-repl--check-source-buffer (buf &optional prefix)
  (if (and (null prefix) (buffer-live-p buf)) buf
    (let ((src-buf
           (nvp:prompt-with-message "Source buffer: "
             :read-fn #'read-buffer
             :read-args (nil t)
             :message (if prefix "Current buffer %S" "Source buffer %S dead") buf)))
      (nvp-repl--set-source (current-buffer) (get-buffer src-buf)))))

(defun nvp-repl-start (&optional prefix)
  "Return a REPL buffer associated with current buffer."
  (nvp-with-repl (init init-async init-use-hook wait process-p buff->proc)
    (cond (init-async (prog1 'async
                        (nvp-repl--make-async-callback (current-buffer) t)))
          (init-use-hook (prog1 'async
                           (nvp-repl--init-with-repl-hooks t)))
          (t (let ((repl-proc (funcall init prefix)) repl-buf)
               (and wait (sit-for wait))
               (unless (funcall process-p repl-proc)
                 (setq repl-buf repl-proc
                       repl-proc (funcall buff->proc repl-buf)))
               (cl-assert (funcall process-p repl-proc))
               (nvp-repl--init-setup (current-buffer) repl-proc repl-buf))))))

(defun nvp-repl-get-buffer (&optional prefix)
  "Return a REPL buffer if one exists, otherwise attempt to start one."
  (nvp-repl-ensure)
  (or (--when-let (nvp-repl-buffer)
        (prog1 it (and prefix (nvp-repl--set-source it (current-buffer)))))
      (nvp-with-repl (process-p proc->buff buff->proc)
        (when-let ((repl-proc (run-hook-with-args-until-success 'nvp-repl-find-functions))
                   (p-buff (if (funcall process-p repl-proc)
                               (funcall proc->buff repl-proc)
                             (setq repl-proc (funcall buff->proc repl-proc)))))
          (if (nvp-repl-live-p repl-proc)  ; found unregistered live one
              (nvp-repl-update repl-proc (current-buffer) p-buff)
            (remhash p-buff nvp-repl--process-buffers))))
      (or (nvp-repl-start prefix)     ; initialize new REPL
          (user-error "Failed to initialize REPL"))))


;; -------------------------------------------------------------------
;;; Commands

;;;###autoload
(defun nvp-repl-jump (&optional prefix)
  "Jump between source and REPL buffers, staring if necessary.
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
  (let* ((repl-p)
         (repl-buff
          (cond ((--when-let (nvp-repl--get-source)
                   (setq repl-p t)
                   (nvp-repl--check-source-buffer it prefix)))
                ((memq major-mode nvp-repl-modes)
                 (setq repl-p t)
                 (nvp-repl-minor-mode 1)
                 (nvp-repl--check-source-buffer nil prefix))
                ((--when-let (nvp-repl-get-buffer prefix)
                   (prog1 it (nvp-repl--source-minor-mode-on))))
                (t nil))))
    (unless (eq 'async repl-buff)
      (pop-to-buffer
       ;; FIXME: when repl is split below, try to pop to source above when it's
       ;; buried somewhere
       repl-buff (nvp-repl--display-action (and repl-p 'other-window))))))

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

(defun nvp-repl-send-string (str &optional insert no-newline)
  "Send STR to current REPL, appending a final newline unless NO-NEWLINE.
If INSERT, STR is inserted into REPL."
  (unless nvp-repl-current
    (user-error "No REPL associated with buffer."))
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

(eval-when-compile
  (defmacro nvp:region (thing)
    `(--when-let (bounds-of-thing-at-point ',thing)
       (list (car it) (cdr it))))

  (cl-defmacro nvp:repl-send ( sender sender-args and-go
                               &rest fallback &key region &allow-other-keys)
    (declare (indent defun))
    (nvp:skip-keywords fallback)
    `(nvp-with-repl ,(if region (seq-uniq (list 'send-region sender))
                       (list sender))
       (if ,sender (funcall-interactively ,sender ,@sender-args)
         ,(if fallback `(progn ,@fallback)
            (if (not region)
                `(user-error
                  ,(concat "Repl doesnt understand '" (symbol-name sender) "'"))
              (macroexp-let2 nil region
                             (if (symbolp region) `(nvp:region ,region)
                               `(delq nil ,region))
                `(progn
                   ,@(when t ; (not (eq sender 'send-region))
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
  "Send region, statement, sexp, or line."
  (interactive "P")
  (cond ((region-active-p)
         (nvp-repl-send-region (region-beginning) (region-end) and-go))
        ((repl:val "send-statement")
         (nvp-repl-send-stmt-or-sentence and-go))
        ((repl:val "send-sexp")
         (nvp-repl-send-sexp and-go))
        (t (nvp-repl-send-line and-go))))

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
  (nvp:repl-send send-buffer () and-go
                 :region (list (nvp-repl--skip-shebang (point-min)) (point-max))))

(defun nvp-repl-send-defun (&optional and-go)
  (interactive "P")
  (nvp:repl-send send-defun () and-go :region defun))

(defun nvp-repl-send-defun-or-region (&optional and-go)
  (interactive "P")
  (cond ((region-active-p)
         (nvp-repl-send-region (region-beginning) (region-end) and-go))
        (t (nvp-repl-send-defun and-go))))

(defun nvp-repl-send-stmt-or-sentence (&optional and-go)
  (interactive "P")
  (nvp:repl-send send-statement () and-go
                 :region (let ((bnds (or (bounds-of-thing-at-point 'statement)
                                         (bounds-of-thing-at-point 'sentence))))
                           (list (car bnds) (cdr bnds)))))

(defun nvp-repl-send-file (file &optional and-go)
  "Send FILE to repl and pop to repl buffer when AND-GO."
  (interactive
   (let* ((file (buffer-file-name))
          (fname (ignore-errors (file-name-nondirectory file))))
     (list (read-file-name "File: " nil file t fname) current-prefix-arg)))
  (nvp:repl-send send-file (file) and-go
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
