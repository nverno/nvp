;;; nvp-macro.el --- compile time macros -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-03-31 05:00:44>
;; Created:  2 November 2016

;;; Commentary:
;; info on declare - defun-declarations-alist, macro-declarations-alist
;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'macroexp)
(require 'nvp-macs-common "macs/nvp-macs-common")
(require 'nvp-macs-setup "macs/nvp-macs-setup")

;; -------------------------------------------------------------------
;;; Internal functions

(defun nvp--normalize-modemap (mode &optional minor)
  "Convert MODE to keymap symbol if necessary.
If MINOR is non-nil, create minor mode map symbol."
  (if (keymapp mode) mode
    (and (symbolp mode) (setq mode (symbol-name mode)))
    (let ((minor (or minor (string-match-p "-minor" mode))))
      (if (not (or (string-match-p "-map\\'" mode)
                   (string-match-p "-keymap\\'" mode)))
          (intern
           (concat (replace-regexp-in-string "\\(?:-minor\\)?-mode\\'" "" mode)
                   (if minor "-minor-mode-map" "-mode-map")))
        (intern mode)))))

(defun nvp--normalize-hook (mode &optional minor)
  "Convert MODE to canonical hook symbol.
If MINOR is non-nil, convert to minor mode hook symbol."
  (and (symbolp mode) (setq mode (symbol-name mode)))
  (let ((minor (or minor (string-match-p "-minor" mode))))
    (intern
     (concat
      (replace-regexp-in-string
       "\\(?:-minor-\\)?\\(?:-mode\\)?\\(?:-hook\\)?\\'" "" mode)
      (if minor "-minor-mode-hook" "-mode-hook")))))

;; -------------------------------------------------------------------
;;; Messages

(defun nvp--msg-from-bindings (bindings &optional prefix)
  "Create message of 'PREFIX: [key] cmd, ...' from list of cons BINDINGS."
  (or prefix (setq prefix "Transient: "))
  (let ((msg (if (listp bindings)
                 (concat
                  prefix
                  (mapconcat (lambda (b)
                               (format "[%S] %S" (car b) (cdr b))) bindings ", "))
               prefix)))
    msg))

(cl-defmacro nvp-msg (fmt &rest args &key keys delay duration &allow-other-keys)
  "Print message, optionally using `substitute-command-keys' if KEYS.
Message is displayed temporarily, restoring any previous message after
DURATION or 2 seconds.
If DELAY is non-nil, message won't be displayed for that many seconds, so
if a useful message is expected it can be read before this message is 
displayed. The original message will be displayed after DELAY + DURATION when
those are both specified."
  (declare (indent defun) (debug (sexp &rest form)))
  (while (keywordp (car args))
    (setq args (cdr (cdr args))))
  (let ((msg
         `(function
           (lambda ()
             (message
              ,@(if keys
                    `("%s" (substitute-command-keys (format ,fmt ,@args)))
                  `(,fmt ,@args)))))))
    `(let ((orig-msg (current-message)))
       ,(if delay
            `(run-with-idle-timer ,delay nil ,msg)
          `(funcall ,msg))
       (run-with-idle-timer
        (+ ,(or delay 0) ,(or duration 2))
        nil (function
             (lambda ()
               (eval
                `(and ,orig-msg (message ,orig-msg)))))))))

;; -------------------------------------------------------------------
;;; General

;; not that useful -- concat only happens one first load
(defmacro nvp-concat (&rest body)
  `(eval-when-compile (concat ,@body)))

(defmacro nvp-re-opt (opts &optional no-symbol)
  `(eval-when-compile
     (concat ,(and (not no-symbol) "\\_<") (regexp-opt ,opts t)
             ,(and (not no-symbol) "\\_>"))))

(defmacro nvp-bfn (&optional no-ext or-buffer)
  "Short buffer file name.
If NO-EXT is non-nil, remove file extension.  If OR-BUFFER is non-nil
use either `buffer-file-name' or `buffer-name'."
  (let ((buff (if or-buffer
                  '(or (buffer-file-name) (buffer-name))
                '(buffer-file-name))))
    `(file-name-nondirectory
      ,(if no-ext `(file-name-sans-extension ,buff)
        buff))))

(cl-defmacro nvp-dfn (&key file-path no-default)
  "Directory name for FILE-PATH (default current buffer or `default-directory').
If NO-DEFAULT is non-nil, don't use `default-directory' if buffer isn't
associated with a file."
  (or file-path (setq file-path 'buffer-file-name))
  `(file-name-nondirectory
    (directory-file-name
     (file-name-directory
      (file-name-sans-extension
       ,(progn
          `(or ,(and file-path `(file-truename ,file-path))
               ,(and (null no-default) '(file-truename default-directory)))))))))

;; -------------------------------------------------------------------
;;; Syntax

(defmacro nvp-ppss ())

(defmacro nvp-in-string (&optional ppss)
  "Non-nil if in string."
  (let ((syn (if (and ppss (symbolp ppss)) ppss (make-symbol "syntax"))))
    `(car (setq ,syn (nthcdr 3 ,(or ppss '(syntax-ppss)))))))

(defmacro nvp-in-comment (&optional ppss in-string)
  "Non-nil if in comment."
  (let ((syn (if (and ppss (symbolp ppss)) ppss (make-symbol "syntax"))))
    (if (and ppss in-string)
        `(car (setq ,syn (cdr ,syn)))
      `(nth 4 ,(or ppss '(syntax-ppss))))))

(defmacro nvp-in-string-or-comment (&optional ppss)
  "Non-nil if in string or comment."
  (macroexp-let2 nil syntax (or ppss '(syntax-ppss))
    `(or (nvp-in-string ,syntax)
         (nvp-in-comment ,syntax 'in-string)
         (nth 3 ,syntax))))

(defmacro nvp-unless-in-comment (&rest body)
  "Execute BODY unless in comment."
  (declare (indent defun) (debug t))
  `(unless (nvp-in-comment)
     ,@body))

(defmacro nvp-if-in-comment (then &rest else)
  "Do THEN if in comment, otherwise ELSE."
  (declare (indent 1) (debug (sexp &rest form)))
  `(if (nvp-in-comment) ,then
     ,@else))

(defmacro nvp-unless-in-string (&rest body)
  "Execute BODY unless in string."
  (declare (indent defun))
  `(unless (nth 3 (syntax-ppss))
     ,@body))

(defmacro nvp-if-in-string (then &rest else)
  "Do THEN if in string, otherwise ELSE."
  (declare (indent 1) (debug (sexp &rest form)))
  `(if (nvp-in-string) ,then
     ,@else))

(defmacro nvp-unless-in-comment-or-string (&rest body)
  "Execute BODY unless currently in a string or comment."
  (declare (indent defun))
  `(unless (nvp-in-string-or-comment)
     ,@body))

(defmacro nvp-if-in-string-or-comment (then &rest else)
  "Do THEN if in string or comment, otherwise ELSE."
  (declare (indent 1) (debug (sexp &rest form)))
  `(if (nvp-in-string-or-comment) ,then
     ,@else))

(defsubst nvp-between-empty-parens-p (&optional point)
  "Non-nil if POINT is between open/close syntax with only whitespace."
  (ignore-errors
    (and point (goto-char point))
    (and
     (progn (skip-syntax-forward " ") (eq ?\) (char-syntax (char-after))))
     (progn (skip-syntax-backward " ") (eq ?\( (char-syntax (char-before)))))))


;; cc-defs
;; #<marker at 34444 in cc-defs.el.gz>

(defmacro nvp-point (position &optional point escape)
  "Return relative POSITION to POINT (default current point).
ESCAPE can be a string to match escaped newlines (default '\\').
POSITION can be one of the following symbols:

`bol'   -- beginning of line
`eol'   -- end of line
`eoll'  -- end of logical line (i.e. without escaped NL)
`bod'   -- beginning of defun
`eod'   -- end of defun
`boi'   -- beginning of indentation
`ionl'  -- indentation of next line
`iopl'  -- indentation of previous line
`bonl'  -- beginning of next line
`eonl'  -- end of next line
`bopl'  -- beginning of previous line
`eopl'  -- end of previous line
`bosws' -- beginning of syntactic whitespace
`eosws' -- end of syntactic whitespace

If the referenced position doesn't exist, the closest accessible point
to it is returned.  This function does not modify the point or the mark."
  (or escape (setq escape "\\\\"))

  (if (eq (car-safe position) 'quote)
      (let ((position (eval position)))
	(cond

	 ((eq position 'bol)
	  (if (not point)
	      '(line-beginning-position)
	    `(save-excursion
	       ,@(if point `((goto-char ,point)))
	       (beginning-of-line)
	       (point))))

	 ((eq position 'eol)
	  (if (not point)
	      '(line-end-position)
	    `(save-excursion
	       ,@(if point `((goto-char ,point)))
	       (end-of-line)
	       (point))))

	 ((eq position 'eoll)
	  `(save-excursion
	     ,@(if point `((goto-char ,point)))
	     (while (progn
		      (end-of-line)
		      (prog1 (eq (logand 1 (skip-chars-backward escape)) 1)))
	       (beginning-of-line 2))
	     (end-of-line)
	     (point)))

	 ((eq position 'boi)
	  `(save-excursion
	     ,@(if point `((goto-char ,point)))
	     (back-to-indentation)
	     (point)))

	 ((eq position 'bod)
	  `(save-excursion
	     ,@(if point `((goto-char ,point)))
             (beginning-of-defun)
             (and defun-prompt-regexp
                  (looking-at defun-prompt-regexp)
                  (goto-char (match-end 0)))
	     (point)))

	 ((eq position 'eod)
	  `(save-excursion
	     ,@(if point `((goto-char ,point)))
             (end-of-defun)
	     (point)))

	 ((eq position 'bopl)
	  (if (not point)
	      '(line-beginning-position 0)
	    `(save-excursion
	       ,@(if point `((goto-char ,point)))
	       (forward-line -1)
	       (point))))

	 ((eq position 'bonl)
	  (if (not point)
	      '(line-beginning-position 2)
	    `(save-excursion
	       ,@(if point `((goto-char ,point)))
	       (forward-line 1)
	       (point))))

	 ((eq position 'eopl)
	  (if (not point)
	      '(line-end-position 0)
	    `(save-excursion
	       ,@(if point `((goto-char ,point)))
	       (beginning-of-line)
	       (or (bobp) (backward-char))
	       (point))))

	 ((eq position 'eonl)
	  (if (not point)
	      '(line-end-position 2)
	    `(save-excursion
	       ,@(if point `((goto-char ,point)))
	       (forward-line 1)
	       (end-of-line)
	       (point))))

	 ((eq position 'iopl)
	  `(save-excursion
	     ,@(if point `((goto-char ,point)))
	     (forward-line -1)
	     (back-to-indentation)
	     (point)))

	 ((eq position 'ionl)
	  `(save-excursion
	     ,@(if point `((goto-char ,point)))
	     (forward-line 1)
	     (back-to-indentation)
	     (point)))

         ;; FIXME: don't use c-skip-* functions that skip
         ;; whitespace/comments/macros
	 ((eq position 'bosws)
	  `(save-excursion
	     ,@(if point `((goto-char ,point)))
	     (c-backward-syntactic-ws)
	     (point)))

	 ((eq position 'eosws)
	  `(save-excursion
	     ,@(if point `((goto-char ,point)))
	     (c-forward-syntactic-ws)
	     (point)))

	 (t (error "Unknown buffer position requested: %s" position))))))

;; paredit splicing reindent doesn't account for prompts
(defmacro nvp-preserving-column (&rest body)
  "Preserve point in column after executing BODY.
`paredit-preserving-column' doesn't properly account for minibuffer prompts."
  (declare (indent defun) (debug body))
  (let ((orig-indent (make-symbol "indentation"))
        (orig-col (make-symbol "column")))
    `(let ((,orig-col (if (eq major-mode 'minibuffer-inactive-mode)
                          (- (current-column) (minibuffer-prompt-width))
                        (current-column)))
           (,orig-indent (current-indentation)))
       (unwind-protect
           (progn ,@body)
         (let ((ci (current-indentation)))
           (goto-char
            (+ (point-at-bol)
               (cond ((not (< ,orig-col ,orig-indent))
                      (+ ,orig-col (- ci ,orig-indent)))
                     ((<= ci ,orig-col) ci)
                     (t ,orig-col)))))))))

;; -------------------------------------------------------------------
;;; Regions / things-at-point

(defmacro nvp-thing-dwim (&optional thing)
  "Thing / region at point DWIM."
  `(cond
    ((use-region-p)
     (buffer-substring-no-properties (region-beginning) (region-end)))
    ((thing-at-point ,(or thing ''symbol) 'noprop))))

(defmacro nvp-bounds-of-thing (thing &optional no-pulse)
  "Return `bounds-of-thing-at-point' and pulse region unless NO-PULSE."
  (declare (indent 0))
  `(progn
     (declare-function nvp-indicate-pulse-region-or-line "")
     (when-let* ((bnds (bounds-of-thing-at-point ,thing)))
       (prog1 bnds
         ,(unless no-pulse
            `(nvp-indicate-pulse-region-or-line (car bnds) (cdr bnds)))))))

(defmacro nvp-region-or-batp (&optional thing no-pulse)
  "Region bounds if active or bounds of THING at point."
  (declare (indent defun) (debug t))
  `(if (region-active-p) (car (region-bounds))
     (nvp-bounds-of-thing (or ,thing 'symbol) ,no-pulse)))

(defmacro nvp-region-str-or-thing (&optional thing no-pulse)
  "Region string if active or THING at point."
  (declare (indent defun) (debug t))
  `(progn
     (declare-function nvp-indicate-pulse-region-or-line "nvp-indicate")
     (if (region-active-p)
         (buffer-substring-no-properties (region-beginning) (region-end))
       (when-let* ((bnds (nvp-bounds-of-thing (or ,thing 'symbol) ,no-pulse))
                   (str (buffer-substring-no-properties (car bnds) (cdr bnds))))
         (if ,no-pulse str
           (prog1 str
             (nvp-indicate-pulse-region-or-line (car bnds) (cdr bnds))))))))

(cl-defmacro nvp-within-bounds-of-thing-or-region
    (thing beg end &rest body &key no-pulse &allow-other-keys)
  "Execute BODY with region widened to bounds of THING at point \
unless region is active.
BEG and END are symbols to bind to the region the bounds.
If NO-PULSE, don't pulse region when using THING."
  (declare (indent defun) (debug (sexp sexp sexp &rest form)))
  (while (keywordp (car body))
    (setq body (cdr (cdr body))))
  `(save-excursion
     (declare-function nvp-indicate-pulse-region-or-line "")
     (if (not (region-active-p))
         (save-restriction
           (widen)
           (when-let* ((bnds (bounds-of-thing-at-point ,thing)))
             (cl-destructuring-bind (,beg . ,end) bnds
               ,(unless no-pulse `(nvp-indicate-pulse-region-or-line ,beg ,end))
               ,@body)))
       ,@body)))

;; -------------------------------------------------------------------
;;; Save/Restore envs 

(defmacro nvp-with-preserved-vars (vars &rest body)
  "Let bind VARS then execute BODY, so VARS maintain their original values.
VARS should be either a symbol or list or symbols."
  (declare (indent 1) (debug (form body)))
  `(cl-progv ,(if (listp vars) `,vars `(list ',vars)) nil
     (unwind-protect
         ,@body)))

;; from yasnippet #<marker at 126339 in yasnippet.el>
(defmacro nvp-letenv (env &rest body)
  "Evaluate BODY with bindings from ENV.
ENV is a lisp expression evaluating to list of (VAR FORM), where
VAR is a symbol and FORM is evaluated."
  (declare (indent 1) (debug (form body)))
  (let ((envvar (make-symbol "envvar")))
    `(let ((,envvar ,env))
       (cl-progv
           (mapcar #'car ,envvar)
           (mapcar (lambda (k-v) (eval (cadr k-v))) ,envvar)
         ,@body))))

;; -------------------------------------------------------------------
;;; Folding

;;-- Hideshow
(defmacro nvp-hs-blocks (start end)
  `(progn
     (setq hs-block-start-regexp ,start)
     (setq hs-block-end-regexp ,end)))

(defmacro nvp-with-hs-block (start end &rest body)
  "Do hideshow with START and END regexps."
  (declare (indent defun))
  `(progn
     (require 'hideshow)
     (unless hs-minor-mode (hs-minor-mode))
     (let ((hs-block-start-regexp ,start)
           (hs-block-end-regexp ,end))
       ,@(or body (list '(hs-toggle-hiding))))))

;; -------------------------------------------------------------------
;;; Control flow

(cl-defmacro nvp-toggled-if (then &rest rest &key this-cmd &allow-other-keys)
  "Do THEN if `last-command' wasn't `this-command', otherwise do REST \
and set `this-command' to nil so opposite happens next time."
  (declare (indent 1))
  (while (keywordp (car rest))
    (setq rest (cdr (cdr rest))))
  `(if (not (eq this-command last-command))
       ,then
     (prog1 (progn ,@rest)
       (setq this-command ,this-cmd))))

;;; FIXME: unused
(defmacro nvp-search-and-go (regexp &optional back &rest body)
  "Search forward or backward for REGEXP, and move point to beginning of
line at match (default) or do BODY at point if non-nil."
  `(let ((start (point)))
     (condition-case nil
         (progn
           (forward-line ,(if back -1 1))
           (,(if back 're-search-backward 're-search-forward) ,regexp)
           ,@(or body (list '(beginning-of-line))))
       (error (goto-char start)))))

;; -------------------------------------------------------------------
;;; Conversions

;;; FIXME: remove -- seem to just occur in C stuff
(defmacro nvp-listify (args)
  "Ensure ARGS is a list."
  (let ((args (if (stringp args) (intern args) args)))
    `(unless (consp ,args) (setq ,args (cons ,args nil)))))

(defmacro nvp-string-or-symbol (sym)
  "If SYM is string convert to symbol."
  `(if (stringp ,sym) (intern ,sym) ,sym))

(defmacro nvp-stringify (name)
  "Sort of ensure that NAME symbol is a string."
  `(progn
     (pcase ,name
       ((pred stringp) ,name)
       ((pred symbolp) (symbol-name ,name))
       (`(quote ,sym) (symbol-name sym))
       (`(function ,sym) (symbol-name sym))
       (_ (user-error "How to stringify %S?" ,name)))))

;; -------------------------------------------------------------------
;;; Programs / Paths
(declare-function nvp-setup-program "nvp-setup")

(defmacro nvp-w32-program (name)
  "Name of cached program on shitty w32.e"
  (and (symbolp name) (setq name (symbol-name name)))
  `(intern (concat "nvp-" ,name "-program")))

;; PATH can contain environment variables to expand
;; if NO-COMPILE is defined the name is determined at runtime
(defmacro nvp-program (name &optional no-compile path)
  (declare (indent defun) (debug t))
  (let* ((name (cond
                ((symbolp name) (symbol-name name))
                ((consp name)
                 (pcase name
                   (`(quote ,sym)
                    (symbol-name sym))
                   (_ name)))
                ((stringp name) name)
                (t (user-error "%S unmatched")))))
    `(progn
       (nvp-declare "" nvp-setup-program)
       (or (,(if no-compile 'progn 'eval-when-compile)
            (nvp-with-gnu/w32
                (let ((exec-path (delq nil (cons ,path '("~/bin/"
                                                         "~/.asdf/shims/"
                                                         "~/.local/bin/"
                                                         "/usr/local/bin/")))))
                  (executable-find ,name))
              (bound-and-true-p (intern (concat "nvp-" ,name "-program"))))
            ;; otherwise try entire PATH
            (executable-find ,name))
           ;; fallback to runtime search
           (when (require 'nvp-setup nil t)
             (nvp-setup-program ,name ,path))))))

(defmacro nvp-path (path &optional no-compile)
  `(,(if no-compile 'progn 'eval-when-compile)
     (let ((path (substitute-env-in-file-name ,path)))
       (and path (file-exists-p path) path))))

(defmacro nvp-mode-config-path (mode &optional ensure-string)
  "Create path for MODE config file."
  `(progn
     (expand-file-name
      (concat "nvp-" ,(if ensure-string (nvp-stringify mode) `,mode)
              "-config.el")
      nvp/config)))

(defmacro nvp-cache-file-path (filename)
  "Create cache path for FILENAME."
  `(progn (expand-file-name ,(nvp-stringify filename) nvp/cache)))

;; -------------------------------------------------------------------
;;; REPLs

;; FIXME: how to make more generic?
(cl-defmacro nvp-hippie-shell-fn (name histfile
                                       &key
                                       (size 5000)
                                       (history ''comint-input-ring)
                                       (bol-fn ''comint-line-beginning-position)
                                       history-fn expand-fn)
  "Setup comint history ring read/write and hippie-expand for it."
  (let ((fn (nvp-string-or-symbol name)))
    `(progn
       (eval-when-compile
         (defvar comint-input-ring-file-name)
         (defvar comint-input-ring-size))
       (declare-function comint-read-input-ring "comint")
       (declare-function comint-write-input-ring "comint")
       (declare-function nvp-he-history-setup "nvp-hippie-history")
       (defun ,fn ()
         (setq comint-input-ring-file-name (expand-file-name ,histfile nvp/cache))
         (setq comint-input-ring-size ,size)
         (comint-read-input-ring)
         (add-hook 'kill-buffer-hook 'comint-write-input-ring nil 'local)
         (nvp-he-history-setup
          :history ,history
          :bol-fn ,bol-fn
          :history-fn ,history-fn
          :expand-fn ,expand-fn)))))

;; switching between REPLs and source buffers -- maintain the name of the
;; source buffer as a property of the process running the REPL. Uses REPL-FIND-FN
;; if supplied to find/create the REPL buffer, REPL-LIVE-P is called to check
;; if it is alive (defaults to `buffer-live-p'
;; if REPL-HISTORY is non-nil `nvp-comint-add-history-sentinel' is added before the
;; buffers process-filter. REPL-INIT is called to create and return a new REPL
;; buffer. REPL-CONFIG is executed in the new REPL buffer after creation
(cl-defmacro nvp-repl-switch (name (&key repl-mode repl-buffer-name repl-find-fn
                                         repl-live-p repl-history repl-process
                                         repl-config repl-wait
                                         repl-doc (repl-switch-fn ''pop-to-buffer))
                              &rest repl-init)
  (declare (indent defun))
  (autoload 'nvp-comint-add-history-sentinel "nvp-comint")
  (let ((fn (intern (format "nvp-%s-repl-switch" name))))
    `(defun ,fn ()
       ,(or repl-doc "Switch between source and REPL buffers")
       (interactive)
       (if ,(or (and repl-mode `(eq major-mode ,repl-mode))
                (and repl-buffer-name `(equal ,repl-buffer-name (buffer-name))))
           ;; in REPL buffer, switch back to source
           (switch-to-buffer-other-window
            ;; switch to set source buffer or the most recent other buffer
            (or (process-get
                 ,(or repl-process '(nvp-buffer-process)) :src-buffer)
                (other-buffer (current-buffer) 'visible)))
         ;; in source buffer, try to go to a REPL
         (let ((src-buffer (current-buffer))
               (repl-buffer
                ;; Should there be a default?
                (or ,(and repl-buffer-name
                          `(get-buffer ,repl-buffer-name))
                    ,(and repl-find-fn
                          `(ignore-errors (funcall ,repl-find-fn))))))
           ;; there is a REPL buffer, but is it alive?
           (when (not (funcall ,(or repl-live-p ''comint-check-proc) repl-buffer))
             ;; no, so we need to start one somehow -- this should return the
             ;; buffer object
             (setq repl-buffer (progn ,@repl-init))
             ,@(and repl-wait `((sit-for ,repl-wait)))
             (and (processp repl-buffer)
                  (setq repl-buffer (process-buffer repl-buffer))
                  ;; add a sentinel to write comint histfile before other
                  ;; sentinels that may be set by the mode
                  ,(and repl-history
                        '(nvp-comint-add-history-sentinel))))
           ;; Now switch to REPL and set its properties to point back to the source
           ;; buffer from whence we came
           (if (not (funcall ,(or repl-live-p ''comint-check-proc) repl-buffer))
               (error (message "The REPL didnt start!!!")))
           ,@(when repl-switch-fn
               `((funcall ,repl-switch-fn repl-buffer)))
           ;; only config first time through
           (when (not (process-get ,(or repl-process
                                        '(get-buffer-process repl-buffer))
                                   :src-buffer))
             ,(and repl-config `(funcall ,repl-config)))
           (process-put ,(or repl-process
                             '(get-buffer-process repl-buffer))
                        :src-buffer src-buffer))))))

;; -------------------------------------------------------------------
;;; Files / Buffers

(defmacro nvp-file-same (file-1 file-2)
  "Return non-nil if FILE-1 and FILE-2 are the same."
  (declare (indent defun))
  `(when (and (file-exists-p ,file-1) (file-exists-p ,file-2))
     (equal (file-truename ,file-1) (file-truename ,file-2))))

;; modified from smartparens.el
(defmacro nvp-with-buffers-using-mode (mode &rest body)
  "Execute BODY in every existing buffer using `major-mode' MODE."
  (declare (indent 1))
  `(dolist (buff (buffer-list))
     (when (provided-mode-derived-p ,mode (buffer-local-value 'major-mode buff))
       (with-current-buffer buff
         ,@body))))

;; -------------------------------------------------------------------
;;; Display Results

(defmacro nvp-with-results-buffer (&optional buffer-or-name &rest body)
  "Do BODY in temp BUFFER-OR-NAME as with `with-temp-buffer-window'.
Make the temp buffer scrollable, in `view-mode' and kill when finished."
  (declare (indent defun) (debug (sexp &rest form)))
  `(let (other-window-scroll-buffer)
     (nvp-window-configuration-save)
     (with-temp-buffer-window
      ,(or buffer-or-name '(help-buffer))
      t
      nil
      (with-current-buffer standard-output
        (setq other-window-scroll-buffer (current-buffer))
        ,@body
        (hl-line-mode)
        (view-mode-enter nil #'nvp-window-configuration-restore)))))

;; evil-with-view-list: #<marker at 154076 in evil-common.el>
(cl-defmacro nvp-with-view-list (&key
                                 name           ;buffer name
                                 mode-name      ;mode-line name
                                 format         ;`tabulated-list-format'
                                 entries        ;`tabulated-list-entries'
                                 select-action) ;function applied to row
  "View buffer in `tabulated-list-mode'."
  (declare (indent defun) (debug t))
  (let ((action (make-symbol "action")))
    `(progn
       (declare-function nvp-view-list-mode "")
       (let ((,action ,select-action)
            (bufname (concat "*" ,name "*"))
            (inhibit-read-only t))
        (and (get-buffer bufname)
             (kill-buffer bufname))
        (let ((buf (get-buffer-create bufname)))
          (with-current-buffer buf
            (setq tabulated-list-format ,format
                  tabulated-list-entries ,entries
                  action ,action)
            (nvp-view-list-mode)        ;inits lists
            (setq mode-name ,mode-name))
          (pop-to-buffer buff))))))

;; -------------------------------------------------------------------
;;; Keys / IO

;; FIXME: which of these is better??
;; `key-description' is defined at C level and `edemacro-format-keys' does
;; a lot of work. How to profile?
;; - semantic-read-event : #<marker at 3072 in fw.el.gz>
(defmacro nvp-last-input-char ()
  "Return the last character input as string."
  '(kbd (substring (edmacro-format-keys (vector last-input-event)) -1)))

(defmacro nvp-last-command-char (&optional strip-ctrl)
  "Return string value from previous command.
If STRIP-CTRL, just return the last character, eg. M-* => *."
  (if strip-ctrl
      `(substring (key-description (vector last-command-event)) -1)
    `(key-description (vector last-command-event))))

;; -------------------------------------------------------------------
;;; Bindings

(defmacro nvp-def-key (map key cmd)
  "Bind KEY, being either a string, vector, or keymap in MAP to CMD."
  (declare (debug t))
  (and (symbolp key) (setq key (symbol-value key)))
  (cl-assert (or (vectorp key) (stringp key) (keymapp key)))
  `(progn
     (declare-function ,cmd "")
     (define-key
      ,(if (keymapp map) `',map map)
      ,(if (or (vectorp key) (keymapp key)) key (kbd key))
      ,(cond
        ((or (null cmd) (and (consp cmd)
                             (or (equal (cdr cmd) '(nil))
                                 (equal (cddr cmd) '(nil)))))
         nil)
        ((consp cmd)
         (cond
          ((equal (car cmd) 'function) `,cmd)
          ((equal (car cmd) 'quote) `#',(cadr cmd))
          (t `,cmd)))
        (t `#',cmd)))))

;;-- Local, Transient, Overriding maps

(cl-defmacro nvp-with-temp-bindings ((&key (keep t) exit bindings)
                                     &rest body)
  "Execute BODY with BINDINGS set in transient map."
  (declare (indent 0))
  (let ((tmap (cl-gensym)))
    `(let ((,tmap (make-sparse-keymap)))
       ,@(cl-loop for (k . b) in bindings
            collect `(nvp-def-key ,tmap ,k ,b))
       (set-transient-map ,tmap ,keep ,exit)
       ,@body)))

(cl-defmacro nvp-use-transient-bindings
    (&optional bindings
               &key
               (keep t)
               (pre '(nvp-indicate-cursor-pre))
               (exit '(lambda () (nvp-indicate-cursor-post)))
               (repeat t) ;add repeat binding as last char
               repeat-key) ;key to use instead of last char
  "Set BINDINGS in transient map or REPEAT command.
If both BINDINGS and REPEAT are nil, do nothing.
Run PRE form prior to setting commands and EXIT on leaving transient map.
If REPEAT is non-nil, add a binding to repeat command from the last input char
or use REPEAT-KEY if specified."
  (declare (indent defun) (debug defun))
  (when (or bindings repeat)            ;w/o one of these, do nothing
    (let ((msg (nvp--msg-from-bindings bindings)))
     `(progn
        (nvp-declare "" nvp-indicate-cursor-pre nvp-indicate-cursor-post)
        ;; only set first time
        (when (null overriding-terminal-local-map)
          (let ((tmap (make-sparse-keymap))
                (repeat-key ,(when repeat (or repeat-key '(nvp-last-command-char)))))
            ,(when repeat
               (prog1 nil
                 (setq msg (concat msg (and bindings ", ") "[%s] repeat command"))))
            (message ,msg repeat-key)   ;echo bindings on first pass
            ,pre
            ,@(cl-loop for (k . b) in bindings
                 collect `(nvp-def-key tmap ,k ,b))
            ,(when repeat '(define-key tmap (kbd repeat-key) this-command))
            (set-transient-map
             tmap
             ,(if repeat `(lambda ()         ;conditions to continue
                            (when (and (not (memq this-command
                                                  '(handle-switch-frame
                                                    keyboard-quit)))
                                       (lookup-key tmap (this-single-command-keys)))
                              (message ,msg repeat-key) t))
                `,keep)
             ,exit)))))))

;; Overrides a minor mode keybinding for the local buffer by creating
;; or altering keymaps stored in buffer-local variable
;; `minor-mode-overriding-map-alist'.
(cl-defmacro nvp-use-minor-mode-overriding-map (mode &rest bindings
                                                     &key predicate
                                                     &allow-other-keys)
  "Override minor MODE BINDINGS using `minor-mode-overriding-map-alist'.
If PREDICATE is non-nil, only override bindings if when it evaluates to non-nil."
  (declare (indent defun))
  (while (keywordp (car bindings))
    (setq bindings (cdr (cdr bindings))))
  `(,@(if predicate `(when ,predicate) '(progn))
    (let ((map (make-sparse-keymap)))
      ,@(cl-loop for (k . b) in bindings
           collect `(nvp-def-key map ,k ,b))
      (push (cons ,mode map) minor-mode-overriding-map-alist))))

(cl-defmacro nvp-set-local-keymap (&rest bindings
                                   &key keymap buffer use &allow-other-keys)
  "Use or create a local version of KEYMAP (default `current-local-map').
If BUFFER is non-nil, use/set BINDINGS locally in BUFFER."
  (declare (indent defun))
  (while (keywordp (car bindings))
    (setq bindings (cdr (cdr bindings))))
  (cl-assert (if (not use) (not (null keymap)) t))
  `(let ((lmap (make-sparse-keymap)))
     (set-keymap-parent lmap ,(or keymap '(current-local-map)))
     ,@(cl-loop for (k . b) in bindings
          collect `(nvp-def-key lmap ,k ,b))
     ,(if buffer `(with-current-buffer ,buffer
                    ,(if use '(use-local-map lmap)
                       `(set (make-local-variable ',keymap) lmap)))
        (if use '(use-local-map lmap)
          `(set (make-local-variable ',keymap) lmap)))))

;;-- Create keymaps

(defmacro nvp-create-keymaps (leader &rest maps)
  "Create submaps from LEADER map. Optionally give name of keymap for
menu entry."
  (declare (indent defun))
  `(progn
     ,@(cl-loop for (key . args) in maps
          as map = (pop args)
          as name = (and args (pop args))
          collect `(progn
                     (eval-when-compile (declare-function ,map "")) ;compiler
                     (define-prefix-command ',map nil ,name)
                     (nvp-def-key ,leader ,key ',map)))))

;;-- Global bindings
(cl-defmacro nvp-global-bindings (&rest bindings &key no-override &allow-other-keys)
  "Add BINDINGS to global keymap.
If NO-OVERRIDE is non-nil, assert that the new binding isn't already defined."
  (declare (indent defun) (debug t))
  (while (keywordp (car bindings))
    (setq bindings (cdr (cdr bindings))))
  (macroexp-progn
   (cl-loop for (k . b) in bindings
      when no-override
      do (when-let ((curr (lookup-key (current-global-map) (kbd k)))))
        (cl-assert t 'show-args (format "%k is assigned %S globally" k curr))
      collect `(nvp-def-key (current-global-map) ,k ,b))))

;;-- General bindings
(cl-defmacro nvp-bind-keys (map &rest bindings
                                &key predicate after-load
                                &allow-other-keys)
  "Add BINDINGS to MAP.
If PRED-FORM is non-nil, evaluate PRED-FROM before binding keys.
If AFTER-LOAD is non-nil, eval after loading feature/file."
  (declare (indent defun) (debug t))
  (while (keywordp (car bindings))
    (setq bindings (cdr (cdr bindings))))
  (let ((map (nvp--normalize-modemap map)))
    `(progn
       (eval-when-compile (defvar ,map))
       (,@(if predicate `(when ,predicate)
            (if after-load `(with-eval-after-load ,after-load) '(progn)))
        ,@(cl-loop for (k . b) in bindings
             collect `(nvp-def-key ,map ,k ,b))))))

(cl-defmacro nvp-bindings (mode &optional feature &rest bindings
                                &key local buff-local minor &allow-other-keys)
  "Set MODE BINDINGS after FEATURE is loaded.
If LOCAL is non-nil, make map buffer local."
  (declare (indent defun))
  (while (keywordp (car bindings))
    (setq bindings (cdr (cdr bindings))))
  (let ((modemap (nvp--normalize-modemap mode minor)))
    `(progn
       (eval-when-compile (defvar ,modemap))
       ,(when local                     ;will change bindings for all mode buffers
          `(make-local-variable ',modemap))
       ,(when buff-local                ;HACK: but how to modify 
          `(with-no-warnings             ; something like `company-active-map'
             (make-variable-buffer-local ',modemap)))
       (with-eval-after-load ,(or feature `',(intern mode))
         ,@(cl-loop for (k . b) in bindings
              collect `(nvp-def-key ,modemap ,k ,b))))))

;;-- View mode default bindings
;; general movement bindings for non-insert modes
(defmacro nvp-bindings-view ()
  ''(("j"     . next-line) ;; use instead of forward-line since it is often advised
     ("k"     . previous-line)
     ("h"     . backward-char)
     ("l"     . forward-char)
     ("e"     . end-of-line)
     ("a"     . beginning-of-line)
     ("A"     . beginning-of-buffer)
     ("E"     . end-of-buffer)
     ("/"     . isearch-forward)
     ("?"     . isearch-backward)
     ("SPC"   . scroll-down)
     ("S-SPC" . scroll-up)
     ("M-n"   . nil)
     ("M-p"   . nil)
     ("M-s-n" . nvp-move-forward-heading)
     ("M-s-p" . nvp-move-previous-heading)
     ("M-N"   . nvp-move-forward-paragraph)
     ("M-P"   . nvp-move-backward-paragraph)))

(defalias 'nvp-bindings-with-view 'nvp-bindings-modal-view)
(defmacro nvp-bindings-modal-view (mode &optional feature &rest bindings)
  (declare (indent defun))
  (let ((bs `(,@(nvp-bindings-view) ,@bindings)))
    `(nvp-bindings ,mode ,feature ,@bs)))

(defmacro nvp-bindings-add-view (mode &optional feature)
  `(progn (nvp-bindings ,mode ,feature ,@(nvp-bindings-view))))

(defmacro nvp-with-view-bindings (&rest body)
  `(nvp-with-temp-bindings
     (:bindings ,@(nvp-bindings-view)
                :keep t
                :exit (lambda () (message "vim out")))
     ,@body))

;;-- Multiple mode bindings
(cl-defmacro nvp-bindings-multiple-modes (modes &rest bindings
                                                &key view &allow-other-keys)
  "Add shared BINDINGS to multiple MODES keymaps.
MODES is of the form ((mode-name . feature) ...).
Optional :local key can be set to make the mappings buffer-local."
  (declare (indent 1))
  (while (keywordp (car bindings))
    (setq bindings (cdr (cdr bindings))))
  `(progn
     ,@(cl-loop for (mode . feature) in modes
          collect (if view
                      `(nvp-bindings-with-view ,mode ',feature ,@bindings)
                    `(nvp-bindings ,mode ',feature ,@bindings)))))

;; -------------------------------------------------------------------
;;; Time

(defmacro nvp-file-older-than-days (file days)
  "non-nil if FILE last modification was more than DAYS ago."
  (declare (indent defun) (debug t))
  `(< (time-to-seconds
       (time-subtract (current-time)
                      (nth 5 (file-attributes ,file))))
      (* 60 60 24 ,days)))

;; Measure and return the running time of the code block.
;; https://github.com/skeeto/.emacs.d/blob/master/lisp/extras.el#L83
(defmacro nvp-measure-time (&rest body)
  (declare (indent defun))
  (garbage-collect)
  (let ((start (make-symbol "start")))
   `(let ((,start (float-time)))
      ,@body
      (- (float-time) ,start))))

;; -------------------------------------------------------------------
;;; Processes

(defmacro nvp-buffer-process (&optional buffer)
  "Return BUFFER's process."
  `(get-buffer-process ,(or buffer '(current-buffer))))

(defalias 'nvp-with-comint-buffer 'nvp-comint-buffer)
(cl-defmacro nvp-comint-buffer (&rest body &key name new result &allow-other-keys)
  "Get a new comint buffer from NAME and execute BODY there, returning buffer.
If NEW is non-nil, use `generate-new-buffer', otherwise `get-buffer-create'.
If RESULT is non-nil, return result of BODY instead of buffer."
  (declare (indent 2) (indent 1) (debug body))
  (while (keywordp (car body))
    (setq body (cdr (cdr body))))
  (or name (setq name "*nvp*"))
  (let ((gen-fn (if new #'generate-new-buffer #'get-buffer-create)))
    `(progn (with-current-buffer (,gen-fn ,name)
              (comint-mode)
              ,(if (not result)
                   `(prog1 (current-buffer)
                      ,@body))))))

(defmacro nvp-with-process-filter (process &optional proc-filter)
  "Run processs with `nvp-proc-default-filter'.
Return process object."
  (declare (indent defun))
  (if (and proc-filter (eql proc-filter :none)) `,process
    (and (not proc-filter) (setq proc-filter ''nvp-proc-default-filter))
    (macroexp-let2* nil ((process process) (proc-filter proc-filter))
      `(prog1 ,process
         (set-process-filter ,process ,proc-filter)))))

(cl-defmacro nvp-with-process-log (process &key
                                           on-error
                                           on-success
                                           proc-filter
                                           (display-action t))
  "Log output in log buffer, if on-error is :pop-on-error, pop to log
if process exit status isn't 0."
  (declare (indent defun))
  (macroexp-let2* nil ((proc `(nvp-with-process-filter ,process ,proc-filter))
                       (on-err (if (keywordp on-error)
                                   ;; (equal on-error :pop-on-error)
                                   `(pop-to-buffer (process-buffer ,proc)
                                                   ,display-action)
                                 on-error)))
    `(progn
       (set-process-sentinel ,proc
                             #'(lambda (p m)
                                 (nvp-log "%s: %s" nil (process-name p) m)
                                 (if (zerop (process-exit-status p))
                                     ,on-success
                                   ,on-error)))
       (display-buffer (process-buffer ,proc) ,display-action))))

(cl-defmacro nvp-with-process
    (process
     &key
     (proc-name process)
     (proc-buff `,(concat "*" proc-name "*"))
     (proc-args nil)
     (proc-filter t)
     (buffer-fn 'get-buffer-create)
     sync
     shell
     sentinel
     (on-success `(progn
                    (nvp-indicate-modeline-success
                     ,(concat " " proc-name " success"))
                    (kill-buffer (current-buffer))))
     (on-failure '(pop-to-buffer (current-buffer))))
  "Start PROCESS with a sentinel doing ON-SUCCESS or ON-FAILURE in process buffer."
  (declare (indent defun) (debug t))
  (let* ((proc (make-symbol "proc"))
         (proc-cmd (intern (format "%s%s"
                                   (if sync "call-process" "start-process")
                                   (if shell "-shell-command" ""))))
         (pbuf (if (and buffer-fn proc-buff) `(,buffer-fn ,proc-buff)
                 `,proc-buff)))
    `(progn
       (declare-function nvp-indicate-modeline "")
       (declare-function nvp-log "")
       (let ((,proc
              ,(if shell
                   (if sync
                       `(funcall
                         ',proc-cmd (mapconcat 'identity (list ,@proc-args) " "))
                     `(funcall ',proc-cmd ,process ,pbuf
                               (mapconcat 'identity (list ,@proc-args) " ")))
                 `(funcall ',proc-cmd
                           ,(or proc-name process)
                           ,pbuf ,process ,@proc-args))))
         ,(if sync `,proc
            ;; Async process filter
            `(progn
               ,(cond
                 ((memq proc-filter '(:default t))
                  `(set-process-filter ,proc 'nvp-proc-default-filter))
                 (proc-filter
                  `(set-process-filter ,proc ,proc-filter))
                 (t nil))
               ;; Process sentinel - just retrun process without sentinel
               ,(cond
                 ((eq sentinel ':default)
                  `(set-process-sentinel ,proc 'nvp-proc-default-sentinel))
                 ((eq sentinel t)
                  `(set-process-sentinel ,proc
                                         ;; FIXME: replace with default
                                         (lambda (p m)
                                           (nvp-log "%s: %s" nil (process-name p) m)
                                           (with-current-buffer (process-buffer p)
                                             (if (zerop (process-exit-status p))
                                                 ,on-success
                                               ,on-failure)))))
                 ((null sentinel) `,proc))))))))

(defmacro nvp-with-process-wrapper (wrapper &rest body)
  "Wrap `set-process-sentinel' to so BODY is executed in environment
where WRAPPER has effect, eg. `cl-letf' will have effect.
Note: use lexical-binding."
  (let ((sps (cl-gensym))
        (proc (cl-gensym))
        (fn (cl-gensym)))
    (macroexp-let2 nil wrapper wrapper
     `(let ((,sps (symbol-function 'set-process-sentinel)))
        (cl-letf (((symbol-function 'set-process-sentinel))
                  (lambda (,proc ,fn)
                    (funcall ,sps ,proc (funcall wrapper ,fn))))
          ,@body)))))

(defmacro nvp-with-async-override (orig-fn new-fn &rest body)
  "Set `symbol-function' of ORIG-FN to NEW-FN in process-buffer of
BODY."
  (declare (indent defun))
  (macroexp-let2 nil new-fn new-fn
   `(with-sentinel-wrapper
     (lambda (fn)
       (let ((fun fn))
         (lambda (p m)
           (cl-letf (((symbol-function ,orig-fn) new-func))
             (funcall fun p m)))))
     ,@body)))

;; -------------------------------------------------------------------
;;; Toggled Tip
(declare-function pos-tip-show "pos-tip")

;; TODO:
;; - use help buffer with xref?
;; - better popup formatting
(cl-defmacro nvp-with-toggled-tip (popup
                                   &key
                                   (help-key "h") ;key-binding for help-fn
                                   (help-fn t)    ;more help (t is default)
                                   bindings       ;additional bindings
                                   (timeout 45)   ;pos-tip timeout
                                   keep           ;keep transient map
                                   use-gtk        ;use gtk tooltips
                                   (help-buffer '(help-buffer)))
  "Toggle POPUP, a help string, in pos-tip.
If HELP-FN is :none, HELP-KEY is not bound by default.
Normally, HELP-KEY triggers a function to jump to a full help description
related to the popup - hopefully in a buffer.
BINDINGS are an alist of (key . function) of additional keys to bind in the
transient keymap.
TIMEOUT is passed to `pos-tip-show'.
If USE-GTK is non-nil use gtk tooltips.
KEEP is passed to `set-transient-map'.
HELP-BUFFER is buffer with full help documentation. This is only applicable to the
default help function."
  (declare (indent defun) (debug t))
  (macroexp-let2* nil ((str popup)
                       (h-key (or help-key "h"))
                       (h-fn (cond
                              ((eq :none help-fn) nil) ;back-compat
                              ((eq t help-fn)
                               `(lambda ()
                                  (interactive)
                                  (x-hide-tip)
                                  (with-help-window ,help-buffer
                                    (with-current-buffer standard-output
                                      (insert ,str)))
                                  ;; (with-current-buffer ,help-buffer
                                  ;;   (insert ,str)
                                  ;;   (view-mode-enter)
                                  ;;   (pop-to-buffer (current-buffer)))
                                  ))
                              (help-fn help-fn)
                              (t nil)))
                       (exit-fn '(lambda () (interactive) (x-hide-tip)))
                       (keep keep))
    `(progn
       (declare-function pos-tip-show "pos-tip")
       (let ((x-gtk-use-system-tooltips ,use-gtk))
         (unless (x-hide-tip)
           (pos-tip-show ,str nil nil nil ,timeout)
           (set-transient-map
            (let ((tmap (make-sparse-keymap)))
              (define-key tmap ,h-key ,h-fn)
              ,@(cl-loop for (k . b) in bindings
                   collect `(define-key tmap ,k ,b))
              tmap)
            ,keep
            ,exit-fn))))))

;; -------------------------------------------------------------------
;;; Building Interactive Functions

;;-- Wrapper functions to call mode-local values
(defmacro nvp-wrapper-function (symbol &optional doc)
  "Creates a simple wrapper function to call local SYMBOL function with \
list of args.
Optionally use DOC for function."
  (let ((fn (intern (string-remove-suffix "-function" (symbol-name symbol))))
        ;; (arg (make-symbol "arg"))
        )
    `(defun ,fn ()
       ,(or doc (format "Function to run local `%s'." symbol))
       (interactive)
       (setq prefix-arg current-prefix-arg)
       (call-interactively ,symbol))))

(defmacro nvp-wrapper-fuctions (&rest fun-docs)
  "Create list of wrapper functions.
FUN-DOCS is an alist of pairs of symbols with optional docs."
  (macroexp-progn
   (cl-loop for (sym . doc) in fun-docs
      collect `(nvp-wrapper-function ,sym ,doc))))

;;-- functions with cached results

;; Simple memoization / result caching
(cl-defmacro nvp-define-cache (func arglist &optional docstring &rest body
                                           &key local predicate cache
                                           &allow-other-keys)
  "Create a simple cache for FUNC results named FUNC or CACHE if non-nil. 
Cache is either defvar (possibly local) so is updated when set to nil,
or PREDICATE is non-nil and returns nil."
  (declare (indent defun) (debug (sexp sexp sexp &form body)) (doc-string 3))
  (while (keywordp (car body))
    (setq body (cdr (cdr body))))
  (let* ((fn (if (stringp func) (intern func) func))
         (cache (or cache fn)))
    `(progn
       ,(if local `(defvar-local ,cache nil)
          `(defvar ,cache nil))
       (defun ,fn ,arglist
         ,docstring
         (or (,@(if predicate `(and ,predicate) '(progn)) ,cache)
             (setq ,cache (progn ,@body)))))))

(defmacro nvp-define-cache-runonce (func arglist &optional docstring &rest body)
  "Define cache function that will only compute cache once."
  (declare (indent defun) (debug defun) (doc-string 3))
  (let ((cache (make-symbol "cache-runonce"))
        (fn (if (stringp func) (intern func) func)))
    `(defun ,fn ,arglist
       ,docstring
       (or (get ',fn ',cache)
           (let ((val (progn ,@body)))
             (prog1 val
               (put ',fn ',cache val)))))))

;;-- marking

;; FIXME: most of these should either be generic or act on local variables
;; instead of being defined many times
;; Marks
(defmacro nvp--mark-defun (&optional first-time &rest rest)
  "Mark blocks, expanding successively."
  `(if (or (and (eq last-command this-command) (mark t))
           (and transient-mark-mode mark-active))
       (set-mark
        (save-excursion
          (goto-char (mark))
          ,@(or rest
                (list
                 '(smie-forward-sexp 'halfsexp)
                 '(point)))))
     ,(or first-time '(mark-defun))))

;;-- Newline

;; FIXME: Obsolete
;; Newline and indents for PAIRS, extends comment region with
;; COMMENT-START when inside COMMENT-RE.
(cl-defmacro nvp-newline (name &optional description
                               &key pairs comment-re comment-start)
  (declare (indent defun))
  (let ((fn (if (symbolp name) name (intern name)))
        (conds
         (cons 'or
               (cl-loop
                  for (open close) in pairs
                  collect `(and (looking-back ,open (line-beginning-position))
                                (looking-at ,close)))))
        (start-re (when comment-re (car comment-re)))
        (end-re (when comment-re (cdr comment-re))))
    `(defun ,fn ()
       ,(or description "Newline dwim.")
       (interactive)
       (let (,@(when pairs `((p ,conds)))
             ,@(when comment-re '((ppss (syntax-ppss)))))
         (cond
          ,@(when comment-re
              `(((and (nth 4 ppss)
                      (save-excursion
                        (forward-line 0)
                        (looking-at-p ,start-re)))
                 ,(when end-re
                    `(when (save-excursion
                             (end-of-line)
                             (looking-back ,end-re (line-beginning-position)))
                       (save-excursion
                         (newline-and-indent))))
                 (newline)
                 (insert ,comment-start)
                 (indent-according-to-mode))))
          (t
           (newline)
           ,@(when pairs
               '((when p
                   (save-excursion
                     (newline-and-indent)))))
           (indent-according-to-mode)))))))

;;-- Wrap

;; Create function to wrap region, inserting BEGIN at beginning,
;; AFTER at the end.
(defmacro nvp-wrap-fn (name doc begin end &optional interact-p)
  (declare (indent defun))
  (let ((fn (intern (if (symbolp name)
                        (symbol-name name)
                      name))))
    `(defun ,fn ()
       ,doc
       ,@(when interact-p
           '(interactive "r"))
       (when (region-active-p)
        (save-excursion
          (goto-char (region-beginning))
          (insert ,begin))
        (goto-char (region-end))
        (insert ,end)))))

;; -------------------------------------------------------------------
;;; URL

(defmacro nvp-path-to-uri (path)
  "Convert PATH to URI."
  `(url-hexify-string
    (concat "file://" (nvp-with-w32 "/") (file-truename ,path))
    url-path-allowed-chars))

(defmacro nvp-uri-to-path (uri)
  "Convert URI to file path."
  `(progn
     (when (keywordp ,uri) (setq ,uri (substring (symbol-name ,uri) 1)))
     (let ((retval (url-filename (url-generic-parse-url (url-unhex-string ,uri)))))
       (nvp-with-gnu/w32 retval (substring retval 1)))))

;; FIXME: Asyncify
(defmacro nvp-with-url-buffer (url &rest body)
  "Do BODY in buffer with contents from URL."
  (declare (indent defun)
           (debug (sexp &rest form)))
  `(with-current-buffer (url-retrieve-synchronously ,url)
     ,@body))

(defmacro nvp-while-scanning-url (url regex &rest body)
  "Do BODY in buffer with URL contents at position of REGEX."
  (declare (indent defun)
           (debug (sexp sexp &rest form)))
  `(nvp-with-url-buffer ,url
     (goto-char (point-min))
     (while (re-search-forward ,regex nil t)
       ,@body)
     (kill-buffer)))

;; -------------------------------------------------------------------
;;; Advice

(defmacro nvp-advise-commands (advice where funcs &optional props)
  "Apply ADVICE at location WHERE to funcs."
  (declare (indent defun))
  `(progn
     ,@(mapcar (lambda (fn) `(advice-add ',fn ,where ,advice ,props)) funcs)))

(defmacro nvp-remove-all-advice (funcs)
  "Remove all advice from list of FUNCS."
  (declare (indent 0))
  `(progn
     ,@(cl-loop for fn in funcs
          collect `(advice-mapc (lambda (advice _props) (advice-remove ',fn advice))
                                ',fn))))

(defmacro nvp-eldoc-function (func &optional init)
  "Set local eldoc function."
  `(progn
     (add-function :before-until (local 'eldoc-documentation-function) #',func)
     ,(when init '(eldoc-mode))))

;; -------------------------------------------------------------------
;;; Mode specific

(defmacro nvp-mode-bind-1 (&optional mode &rest bindings)
  "Attach BINDINGS globally to MODE."
  (declare (indent defun) (debug bindings))
  (or mode (setq mode (quote major-mode)))
  (unless (equal 'quote (car-safe mode)) (setq mode `',mode))
  `(if (not (get ,mode 'nvp))
       (put ,mode 'nvp ,@bindings)
     (put ,mode 'nvp
          (cl-delete-duplicates
           (append (get ,mode 'nvp) ,@bindings) :key #'car))))

(defmacro nvp-mode-bind (&optional modes &rest bindings)
  "Attach BINDINGS globally to MODES."
  (declare (indent defun) (debug t))
  ;; Allow for some various forms
  (unless (null (cadr (car-safe bindings)))
    (and (equal 'quote (car-safe modes))
         (setq modes (cadr modes)))                       ; '(a b c)
    (unless (listp modes) (setq modes (cons modes nil)))  ; some-mode
    (setq modes (remq 'quote modes))                      ; ('mode-1 'mode-...)
    (macroexp-progn
     (cl-loop for mode in modes
        collect `(nvp-mode-bind-1 ,mode ,@bindings)))))

;; -------------------------------------------------------------------
;;; Package

(defmacro nvp-load-file-name ()
  "Expand to the file's name."
  '(cond
    (load-in-progress load-file-name)
    ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
     byte-compile-current-file)
    (t (buffer-file-name))))

(defmacro nvp-package-root (&optional name)
  "Expand to the default package directory with default prefix or NAME."
  (let ((prefix
         (if name
             (if (symbolp name) (symbol-name name) name)
           (file-name-nondirectory
            (directory-file-name
             (file-name-directory (nvp-load-file-name)))))))
    (intern (concat prefix "--dir"))))

(cl-defmacro nvp-package-define-root (&key name snippets dirs after-load)
  "Define package root directory with default prefix as directory name or NAME.
If SNIPPETS is non-nil, setup snippet loading for directory.
If DIRS is non-nil it should be a list of variables to define as directories
relative to the project root directory as symbols 'prefix--dir'.
AFTER-LOAD is a form to execute after file is loaded during which the root
directory is bound to `root' and all `dirs' are let-bound to their symbols."
  (declare (indent 0) (debug t))
  (let* ((file (cond
                (load-in-progress load-file-name)
                ((and (boundp 'byte-compile-current-file)
                      byte-compile-current-file)
                 byte-compile-current-file)
                (t (buffer-file-name))))
         (root-val (file-name-directory file))
         (base (file-name-nondirectory (directory-file-name root-val)))
         (prefix (if name (if (symbolp name) (symbol-name name) name) base))
         (root (intern (concat prefix "--dir")))
         (dirs (mapcar (lambda (d)
                         (and (symbolp d) (setq d (symbol-name d)))
                         (list (intern d)
                               (intern (concat prefix "--" d))
                               (expand-file-name d root-val)))
                       dirs))
         (mappings (cons `(root ,root) (mapcar 'butlast dirs))))
    `(progn
       (eval-and-compile
         (defconst ,root ,root-val))
       ,(when snippets `(nvp-package-load-snippets ,root))
       ,(when dirs
          `(progn
             ,@(cl-loop for (_orig-sym dir-sym dir-val) in dirs
                  collect `(defconst ,dir-sym ,dir-val))))
       ,(when after-load
          `(with-eval-after-load ,file
             (let ,mappings
               ,after-load))))))

(defmacro nvp-package-load-snippets (dir)
  "Add packages snippet directory to `yas-snippet-dirs' after loading
`yasnippet'."
  `(progn
     (eval-when-compile (defvar yas-snippet-dirs))
     (declare-function yas-load-directory "yasnippet")
     (with-eval-after-load 'yasnippet
       (let ((snippet-dir (expand-file-name "snippets" ,dir))
             (dirs (or (and (consp yas-snippet-dirs) yas-snippet-dirs)
                       (cons yas-snippet-dirs ()))))
         (unless (member snippet-dir dirs)
           (setq yas-snippet-dirs (delq nil (cons snippet-dir dirs))))
         (yas-load-directory snippet-dir)))))

;; -------------------------------------------------------------------
;;; Setup / Build init

;;-- Setup helper functions
;; Find locations for init constants
(defun nvp--setup-normalize-locs (locs &optional defaults)
  "Ensure LOCS is a list.
If LOCS is nil, use DEFAULTS.  If it is a symbol/function (list) get its value(s)."
  (if (null locs)
      (or defaults (nvp-with-gnu/w32 '("~/") '("~/" "d:/" "c:/")))
    (if (and (consp locs) (functionp (car locs)))
        (list (eval locs))
      (and (not (listp locs)) (setq locs (cons locs nil)))
      (mapcar (lambda (l)
                (cond
                 ((consp l) l)
                 ((symbolp l) (symbol-value l))
                 (t l)))
              locs))))

(defun nvp--setup-find-loc (locs &optional places file)
  "Find first existing location in LOCS."
  (let ((locs (nvp--setup-normalize-locs locs nil))
        (places (nvp--setup-normalize-locs places)))
    (cl-loop for loc in locs
       return (cl-loop for place in places
                 as root = (if (symbolp place) (symbol-value place) place)
                 as loc-name = (expand-file-name loc root)
                 when (file-exists-p loc-name)
                 return (if file (directory-file-name loc-name)
                          (file-name-as-directory loc-name))))))

(defun nvp--setup-subdirs (root &optional ignored)
  (setq root
        (cond
         ((symbolp root) (setq root (symbol-value root)))
         ((consp root) (eval root))))
  (cl-remove-if
   (lambda (f)
     (or (not (file-directory-p f))
         (cl-member (file-name-nondirectory f) ignored :test 'string=)))
   (directory-files root t "^[^.]")))

;;-- setup macros
(cl-defmacro nvp-add-to-alist (&rest items
                                     &key
                                     (alist 'auto-mode-alist)
                                     (test 'equal)
                                     &allow-other-keys)
  "Add ITEMS, a list of cons cells, to ALIST using TEST to check if item \
is already present."
  (declare (indent defun) (debug t))
  (while (keywordp (car items))
    (setq items (cdr (cdr items))))
  (macroexp-progn
   (cl-loop for (k . v) in items
      with lst = (if (consp alist) alist (symbol-value alist))
      unless (cl-member (cons k (quote v)) lst :test test)
      collect `(push (cons ,k ',v) ,alist))))

(defmacro nvp-setup-diminish (&rest modes)
  "Diminish MODES in modeline.
MODES is of form (feature . mode)."
  (declare (indent 0))
  `(progn
     (eval-when-compile ,@(mapcar (lambda (f) `(defvar ,(cdr f))) modes))
     ,(macroexp-progn
       (cl-loop for (feat . mode) in modes
          collect `(eval-after-load ',feat '(diminish ',mode))))))

(defmacro nvp-setup-consts (&rest vars)
  "Define consts in init."
  (declare (indent 0) (debug t))
  (macroexp-progn
   (cl-loop for (v dir places file) in vars
      as loc = (nvp--setup-find-loc dir places file)
      do (eval `(defconst ,v ,loc)) ;so subsequent vars can use
      collect `(defconst ,v ,loc))))

(defmacro nvp-setup-load-files (&rest files)
  (declare (indent 0))
  (macroexp-progn
   (cl-loop for f in files
      collect `(load ,f 'noerror 'nomessage))))

(defmacro nvp-setup-load-paths (&rest paths)
  (declare (indent 0))
  (macroexp-progn
   (cl-loop for p in paths
      collect `(add-to-list 'load-path ,p))))

(defmacro nvp-setup-hooks (hook &rest modes)
  "Add HOOK to all MODES hooks."
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for mode in modes
      as mode-hook = (nvp--normalize-hook mode)
      collect `(add-hook ',mode-hook #',hook))))

(defmacro nvp-setup-add-subdir-load-paths (root &optional ignored)
  "Add subdirs under ROOT to `load-path', ingnoring those listed in IGNORED."
  (declare (indent defun))
  (let* ((ignored (or ignored '("." ".." "unused" "ignored" "old")))
         (dirs (nvp--setup-subdirs root ignored)))
    (macroexp-progn
     (cl-loop for d in dirs
        collect `(add-to-list 'load-path ,d)))))

(defmacro nvp-setup-cache (var filename)
  "Set cache FILENAME location."
  `(nvp-setq ,var (expand-file-name ,filename nvp/cache)))

;; -------------------------------------------------------------------
;;; Other modes

;;-- Hydras
(defmacro nvp-hydra-set-property (hydra-name &rest props)
  "Apply PROPS to HYDRA-NAME after `hydra' is loaded.
PROPS defaults to setting :verbosity to 1."
  (declare (indent 1))
  (unless props (setq props (list :verbosity 1)))
  `(progn
     (declare-function hydra-set-property "hydra")
     (with-eval-after-load 'hydra
      ,@(cl-loop for (k v) on props by #'cddr
           collect `(hydra-set-property ,hydra-name ,k ,v)))))

;;-- Smartparens
(cl-defmacro nvp-sp-local-pairs (&rest pairs &key modes &allow-other-keys)
  (declare (indent defun) (debug defun))
  (while (keywordp (car pairs))
    (setq pairs (cdr (cdr pairs))))
  `(progn
     (eval-when-compile (require 'smartparens))
     (declare-function sp-local-pair "smartparens")
     ,(cond
       (modes
        `(sp-with-modes ,modes
           ,@pairs))
       ((equal 'quote (caar pairs)) `(sp-local-pair ,@pairs))
       (t
        (macroexp-progn
         (cl-loop for pair in pairs
            collect `(sp-local-pair ,@pair)))))))

;; -------------------------------------------------------------------
;;; Locals - silence compiler

(defmacro nvp-local-vars ()
  '(progn
     (defvar nvp/abbrevs)
     (defvar nvp/auto)
     (defvar nvp/auto-site)
     (defvar nvp/devel)
     (defvar nvp/site)
     (defvar nvp/modes)
     (defvar nvp/emacs)
     (defvar nvp/build)
     (defvar nvp/project)
     (defvar nvp/info)
     (defvar nvp/bin)
     (defvar nvp/binw)
     (defvar nvp/msys)
     (defvar nvp/cygwin)
     (defvar nvp/vms)
     (defvar nvp/git)
     (defvar nvp/test)
     (defvar nvp/lisp)
     (defvar nvp/config)
     (defvar nvp/custom)
     (defvar nvp/data)
     (defvar nvp/template)
     (defvar nvp/snippet)
     (defvar nvp/class)
     (defvar nvp/work)
     (defvar nvp/bookmark)
     (defvar nvp/cache)
     (defvar nvp/backup)
     (defvar nvp/org)
     (defvar nvp/books)
     (defvar nvp/install)
     (defvar nvp/private)))

(nvp-local-vars)

;; Doesn't work
;; (put 'require 'byte-hunk-handler 'byte-compile-file-form-require)
(when (functionp 'backtrace-frames)
  (when (assoc '(t byte-compile-file-form-require
                   ((require 'nvp-macro))
                   nil)
               (backtrace-frames))
    (message "Warning: package 'nvp-macro required at runtime")))

(provide 'nvp-macro)
;;; nvp-macro.el ends here
