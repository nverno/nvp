;;; nvp-macro.el --- compile time macros -*- lexical-binding: t; -*-

;;; Commentary:

;; **only required at compile time**
;; declare forms: defun-declarations-alist, macro-declarations-alist
;; byte comp: byte-compile-macro-environment

;;; Code:
(require 'cl-lib)
(require 'lisp-mode) ; `let-when-compile'
(require 'subr-x)
(require 'macroexp)
(require 'inline)
(require 'dash)
(require 'hydra)
(require 'cc-defs)
(require 'seq)
(require 'nvp-macs-common "macs/nvp-macs-common")
(require 'nvp-macs-setup "macs/nvp-macs-setup")
(require 'nvp-macs-bindings "macs/nvp-macs-bindings")
(require 'nvp-macs-process "macs/nvp-macs-process")
(require 'nvp-macs-decls "macs/nvp-macs-decls")
(require 'nvp-macs-fonts "macs/nvp-macs-fonts")
(require 'nvp-macs-display "macs/nvp-macs-display")
(require 'nvp-subrs "macs/nvp-subrs")

;; add compile time paths interactively
(cl-eval-when (load eval)
  (dolist (subdir '("macs" "subrs"))
    (let ((dir (expand-file-name
                subdir
                (file-name-directory (nvp:load-file-name)))))
      (add-to-list 'load-path dir))))

;;; Local Variables
(defmacro nvp:ensure-local-variables ()
  "Make sure directory local variables have been read, even in shell."
  `(when (derived-mode-p 'comint-mode)
     (hack-local-variables)))

;;; Major Mode
(defmacro nvp:major-mode (&optional mode original)
  "If ORIGINAL, and MODE (default `major-mode') was remapped, return remapped
mode."
  (setq mode (if mode `',mode 'major-mode))
  (if original
      `(or (car (rassq ,mode major-mode-remap-alist)) ,mode)
    `,mode))

;;; Fallback
(cl-defmacro nvp:with-fallback (&rest body &key fallback minibuffer-fallback
                                      &allow-other-keys)
  "Catch \\='nvp-fallback in BODY.
If `nvp-exit' is set to \\='fallback during BODY, call either FALLBACK or
`nvp-fallback-function', if they are non-nil on result of BODY.
`nvp-fallback-minibuffer-function' is bound to MINIBUFFER-FALLBACK when non-nil."
  (declare (indent defun) (debug t))
  (nvp:skip-keywords body)
  (nvp:with-syms (res)
    `(let* (,@(if fallback `((nvp-fallback-function ,`,fallback)))
            ,@(if minibuffer-fallback
                  `((nvp-fallback-minibuffer-function ,`,minibuffer-fallback)))
            (nvp-exit nil)
            (,res (catch 'nvp-fallback (progn ,@body))))
       (if (and (eq nvp-exit 'fallback)
                (bound-and-true-p nvp-fallback-function))
           (funcall nvp-fallback-function ,res)
         ,res))))

;; -------------------------------------------------------------------
;;; Prefix args

;; parse symbols like '>12 to (> . 12)
(defsubst nvp--prefix-parse-testsym (sym)
  "Parse symbols, SYM, like \\='>12 to (> . 12)."
  (when (and (not (numberp sym)) (not (listp sym)) (symbolp sym))
    (--when-let (symbol-name sym)
      (when (string-match "\\([><=]+\\)\\([0-9]+\\)" it)
        (let ((test (match-string 1 it))
              (num (match-string 2 it)))
          (cons (and test (intern test))
                (and num (string-to-number num))))))))

(cl-defmacro nvp:prefix (num &optional then &rest else &key test &allow-other-keys)
  "If `current-prefix-arg' equals NUM, using TEST, do THEN otherwise ELSE."
  (declare (indent defun) (debug t))
  (nvp:skip-keywords else)
  (and test (setq test (car (nvp:list-unquote test))))
  (--when-let (and (eq (car-safe num) 'quote)
                   (nvp--prefix-parse-testsym (cadr num)))
    (cl-assert (not test) nil
               "nvp:prefix called with two tests: %S, %S" num test)
    (setq num (cdr it)
          test (car it)))
  (let ((test-fn (if test
                     `(,test (prefix-numeric-value current-prefix-arg) ,num)
                   (if (listp num)
                       (if (eq nil num)
                           '(not current-prefix-arg)
                         `(memq (prefix-numeric-value current-prefix-arg) ,num))
                     `(eq (prefix-numeric-value current-prefix-arg) ,num)))))
    (if (or then else)
        `(if ,test-fn
             ,then
           ,@else)
      test-fn)))

;; -------------------------------------------------------------------
;;; Transient

(defmacro nvp:def-transient-toggle-vars (menu &rest vars)
  "Define infix toggles on transient MENU for VARS."
  (declare (indent defun))
  (macroexp-progn
   (cl-loop
    for var in vars
    collect (let ((name
                   (concat (symbol-name menu) "--toggle-" (symbol-name var))))
              `(transient-define-infix ,(intern name) ()
                 :class 'transient-lisp-variable
                 :variable ',var
                 :reader (lambda (&rest _) (not ,var)))))))

;; -------------------------------------------------------------------
;;; Repeat

(defmacro nvp:repeat-this-command (&optional key no-indicate)
  "Repeat `this-command' on last basic input or KEY.
Don't change cursor when NO-INDICATE."
  (let ((repeat-key (or key `(nvp:input 'lbi))))
    `(when (and (null overriding-terminal-local-map)
                (not (memq this-command `(,last-command nvp--repeat))))
       ,@(unless no-indicate '((nvp-indicate-cursor-pre)))
       (set-transient-map
        (let ((map (make-sparse-keymap)))
          (define-key map (vector ,repeat-key) this-command)
          map)
        t
        ,@(unless no-indicate '((lambda () (nvp-indicate-cursor-post))))))))

;; -------------------------------------------------------------------
;;; Output / Messages

(nvp:decls :f (nvp-window-configuration-restore nvp-window-configuration-save))

(cl-defmacro nvp:with-results-buffer ( &rest body
                                       &key buffer title revert-fn
                                       (save-windows t)
                                       (restore-windows t)
                                       (action t)
                                       &allow-other-keys)
  "Do BODY in temp BUFFER-OR-NAME as with `with-temp-buffer-window'.
Make the temp buffer scrollable, in `view-mode' and kill when finished.
Add TITLE to results buffer."
  (declare (indent defun) (debug (sexp &rest form)))
  (nvp:skip-keywords body)
  (macroexp-let2 nil hdr (if title `(nvp:centered-header ,title))
    `(let (other-window-scroll-buffer)
       ,(when save-windows '(nvp-window-configuration-save))
       (with-temp-buffer-window
           ,(or buffer '(help-buffer))
           ,(if (eq ':none action) nil action)
           nil
         (with-current-buffer standard-output
           (setq other-window-scroll-buffer (current-buffer))
           ,(if title `(princ ,hdr))
           ,@body
           (hl-line-mode)
           (view-mode-enter
            nil ,(and restore-windows '#'nvp-window-configuration-restore))
           ,@(when revert-fn
               `((setq-local revert-buffer-function
                             (lambda (&rest args)
                               ,(and save-windows '(nvp-window-configuration-restore))
                               (funcall ,revert-fn args)
                               (pop-to-buffer ,(or buffer '(help-buffer)))))
                 (nvp-buffer-local-set-minor-mode-key 'view-mode "G" #'revert-buffer))))))))


(cl-defmacro nvp:with-tabulated-list (&rest body &key name format entries action
                                            &allow-other-keys)
  "View results in buffer NAME in `tabulated-list-mode'.
FORMAT and ENTRIES define `tabulated-list-format' and `tabulated-list-entries'
to use.
If ACTION is non-nil, `nvp-tabulated-list-select-action' will be bound to it,
which will be called when selecting an entry.
The rest of BODY is evaluated in result buffer after `tabulated-list-mode'
is activated, but before items are printed."
  (declare (indent defun) (debug t))
  (nvp:skip-keywords body)
  `(progn
     (let ((bufname (concat "*" ,name "*"))
           (inhibit-read-only t))
       (and (get-buffer bufname)
            (kill-buffer bufname))
       (let ((buf (get-buffer-create bufname)))
         (with-current-buffer buf
           (setq tabulated-list-format ,format
                 tabulated-list-entries ,entries)
           (tabulated-list-mode)
           ;; allow for further customization of tabulated list mode
           ,@body
           (tabulated-list-init-header)
           (tabulated-list-print)
           (setq mode-name ,name)
           ,(when action `(setq nvp-tabulated-list-select-action ,action)))
         (pop-to-buffer buf)))))

(cl-defmacro nvp:msg-repeated (fmt &rest args &key clobber &allow-other-keys)
  "Construct minibuffer message by applying `message' to FMT with ARGS.
Message is appended to current minibuffer messages and updated on repeated
calls.
CLOBBER can be specify a message prefix to clobber instead of appending to."
  (declare (indent defun))
  (unless (stringp fmt) (setq fmt (eval fmt)))
  (cl-assert (stringp fmt))
  (let* ((prefix (car (string-split fmt "%")))
         (repeat-prefix (concat "[" prefix))
         (replace (concat "\\[" prefix ".*\\'")))
    (nvp:skip-keywords args)
    (nvp:with-syms (msg cur)
      `(unless (minibufferp)
         (let ((message-log-max nil)
               (,cur (current-message))
               (,msg (funcall #'format ,fmt ,@args)))
           (if ,cur
               (cond
                (,(if clobber `(or (string-prefix-p ,prefix ,cur t)
                                   (string-prefix-p ,clobber ,cur t))
                    `(string-prefix-p ,prefix ,cur t))
                 (message ,msg))
                ((string-search ,repeat-prefix ,cur)
                 (message
                  "%s [%s]" (replace-regexp-in-string ,replace "" ,cur) ,msg))
                (t (message "%s [%s]" ,cur ,msg)))
             (message ,msg)))))))


(cl-defmacro nvp:msg (fmt &rest args
                          &key test delay duration append clobber
                          &allow-other-keys)
  "Print message FMT with ARGS.

If TEST is non-nil, message will only be displayed if it evaluates
truthy (if TEST is a function it will be called with no arguments).

If APPEND, message is appended to any current message.
Otherwise, message is displayed temporarily, restoring any previous message
after DURATION or 2 seconds.

If DELAY is non-nil, message is delayed for that many seconds, eg. allowing
previous messages to be read.

The original message will be restored after DELAY + DURATION when those
are both specified."
  (declare (indent defun) (debug (sexp &rest form)))
  (nvp:skip-keywords args)
  (macroexp-let2 nil message `(format (substitute-command-keys ,fmt) ,@args)
    (nvp:with-syms (orig-msg)
      (let ((do-msg
             `(let ((message-log-max nil))
                (or (minibufferp)
                    ,@(when test
                        `((not ,(if (functionp test) `(funcall ,test) `,test))))
                    ,(if append
                         `(message
                           (if-let ((cur (current-message)))
                               ,(if clobber
                                    `(if (string-prefix-p ,clobber cur)
                                         ,message
                                       (concat cur " [" ,message "]"))
                                  `(concat cur " [" ,message "]"))
                             ,message))
                       `(message ,message))))))
        (if append do-msg
          (let ((msg `(function (lambda () ,do-msg)))
                (post-msg
                 `(function
                   (lambda (orig-msg)
                     (and orig-msg (not (minibufferp)) (message orig-msg))))))
            `(let ((,orig-msg (current-message)))
               ,(if delay
                    `(run-with-idle-timer ,delay nil ,msg)
                  `(funcall ,msg))
               (and ,orig-msg
                    (run-with-idle-timer
                     ,(+ (or delay 0) (or duration 2)) nil
                     (apply-partially ,post-msg ,orig-msg))))))))))

;; `org-no-popups'
(defmacro nvp:with-no-popups (&rest body)
  "Suppress popup windows in BODY."
  `(let (pop-up-frames display-buffer-alist)
     ,@body))

(defmacro nvp:prompt-with-message (prompt &optional format-string &rest args)
  "Display message in mode-line while reading PROMPT from minibuffer.
Message uses FORMAT-STRING and ARGS."
  (let ((read-fn 'read-from-minibuffer))
    (while (keywordp (car args))
      (if (eq ':read-fn (car args))
          (setq read-fn (car (nvp:list-unquote (cadr args)))))
      (setq args (cddr args)))
    `(progn
       (require 'eldoc)
       (minibuffer-with-setup-hook
           (:append (lambda () (eldoc-minibuffer-message ,format-string ,@args)))
         (,read-fn ,prompt)))))


;; -------------------------------------------------------------------
;;; Input

;; `magit-read-char-case'
(defmacro nvp:read-char-case (prompt verbose &rest clauses)
  "PROMPT for choice of CLAUSES. VERBOSE displays options."
  (declare (indent 2) (debug (form form &rest (characterp form body))))
  `(prog1 (pcase (read-char-choice
                  ,(concat prompt
                           (concat (mapconcat 'cadr clauses ", ")
                                   (and verbose ", or [C-g] to abort") " "))
                  ',(mapcar 'car clauses))
            ,@(--map `(,(car it) ,@(cddr it)) clauses))
     (message "")))

;; -------------------------------------------------------------------
;;; Completion

(nvp:decl vertico--exhibit)
(nvp:local-defvars vertico--input vertico--history-hash vertico--lock-candidate)

;;-- vertico
(defmacro nvp:vertico-update-candidates (reset &rest update)
  "Update completion candidates and vertico display.
RESET resets vertico candidates.
UPDATE is used to create `minibuffer-completion-table'."
  (declare (indent defun) (debug t))
  `(progn
     ,(and update
           `(setq minibuffer-completion-table (progn ,@update)))
     (when vertico--input
       (setq vertico--input t)
       ,(when reset
          `(setq vertico--history-hash nil
                 vertico--lock-candidate nil))
       (vertico--exhibit))))

;; -------------------------------------------------------------------
;;; Strings / Regexps

;; stolen from `magit-bind-match-strings'
(defmacro nvp:bind-match-strings (varlist string &rest body)
  "Bind variables to submatches according to VARLIST then evaluate BODY.
Bind the symbols in VARLIST to submatches of the current match
data, starting with 1 and incrementing by 1 for each symbol.  If
the last match was against a string, then that has to be provided
as STRING."
  (declare (indent 2) (debug (listp form body)))
  (let ((s (cl-gensym "string"))
        (i 0))
    `(let ((,s ,string))
       (let ,(save-match-data
               (--map (list it (list 'match-string (cl-incf i) s)) varlist))
         ,@body))))


;; -------------------------------------------------------------------
;;; Buffer / Directory names
;;
;; others:
;; - `consult--local-let' :: buffer local let bind dynamic variables

(cl-defmacro nvp:visible-windows (&key mode derived test-fn)
  "List buffers visible in current frame.
If MODE is non-nil, only return buffers with matching (or DERIVED)
\\='major-mode. Otherwise if TEST-FN, is non-nil only list buffers where
\\='(TEST-FN buffer) is non-nil."
  (declare (indent defun) (debug t))
  (when (and mode test-fn) (error "nvp:visible-windows: %S is ignored" test-fn))
  (when (consp mode)
    (setq mode (nvp:list-unquote mode)))
  (macroexp-let2 (lambda (e) (or (null e) (consp e))) mode mode
    `(let (res)
       (walk-windows
        (lambda (w)
          ,(if (or mode test-fn)
               `(let* ((buff (window-buffer w))
                       ,@(when derived
                           '((major-mode
                              (buffer-local-value 'major-mode buff)))))
                  (and ,(if mode
                            (if derived
                                (if (listp mode)
                                    `(derived-mode-p ,@(--map `(quote ,it) mode))
                                  `(derived-mode-p ,mode))
                              `(memq
                                (buffer-local-value 'major-mode buff)
                                ,(if (listp mode) `',mode
                                   `(if (listp ,mode) ,mode (list ,mode)))))
                          `(funcall ,test-fn buff))
                       (push w res)))
             `(push w res)))
        nil 'visible)
       res)))

(defmacro nvp:buff--1 (path no-def)
  (if no-def
      `(file-name-directory (file-truename ,path))
    `(or (file-name-directory (file-truename ,path))
         (file-truename default-directory))))

(defmacro nvp:buff--2 (buff &optional or-name)
  (if or-name
      `(or (buffer-file-name ,buff) (buffer-name ,buff))
    `(buffer-file-name ,buff)))

(cl-defmacro nvp:path (type &optional b-o-p &key no-default or-name)
  "Return buffer or path names.
If B-O-P is non-nil, it is used as the buffer or path instead of the current
buffer, buffer-file, or default directory.
If NO-DEFAULT is non-nil, don't use `default-directory' if `buffer-file-name'
is nil.
If OR-NAME is non-nil, use `buffer-name' if `buffer-file-name' is nil.

* ~~~ Buffers
`bn'     -- Buffer name
`bf'     -- Buffer file name
`bfa'    -- Buffer file name abbreviated
`bfs'    -- Buffer file Short name (no directory)
`bfe'    -- Buffer file name w/o extension
`bfod'   -- Buffer file name or default directory
`bfse'   -- Buffer file short name w/o extension

* ~~~ Directories
`ds'     -- Directory's short name (just the containing directory)
`dn'     -- Full name of directory containing buffer file (no trailing slash)
`dn/'    -- Same, with trailing slash
`dna'    -- Directory name abbreviated
`dna/'   -- Same, with trailing slash

* ~~~ Paths
`fa'     -- Abbreviated file name
`fs'     -- Short file name (no directory)
`fse'    -- same, w/o extension
`ext'    -- File extension
`exp'    -- Try to substitute environment variables in path, otherwise try to
            expand path again when it is loaded."
  (let ((type (eval type)))
    (cond
     ;; buffer files
     ((eq type 'bn) `(buffer-name ,b-o-p))
     ((eq type 'bf) `(nvp:buff--2 ,b-o-p ,or-name))
     ((eq type 'bfa) `(abbreviate-file-name (nvp:buff--2 ,b-o-p)))
     ((eq type 'bfod) `(or (nvp:buff--2 ,b-o-p ,or-name) default-directory))
     ((eq type 'bfs) `(file-name-nondirectory (nvp:buff--2 ,b-o-p ,or-name)))
     ((eq type 'bfe) `(file-name-sans-extension (nvp:buff--2 ,b-o-p ,or-name)))
     ((eq type 'bfse) `(file-name-base (nvp:buff--2 ,b-o-p ,or-name)))

     ;; directories
     ((eq type 'ds)                     ; containing directory's short name
      (if (not b-o-p)
          `(file-name-nondirectory
            (directory-file-name
             (nvp:buff--1 buffer-file-name ,no-default)))
        `(if (directory-name-p ,b-o-p)
             (file-name-nondirectory
              (directory-file-name (file-truename ,b-o-p)))
           (file-name-nondirectory
            (directory-file-name
             (file-name-directory (file-truename ,b-o-p)))))))

     ((eq type 'dn)                     ;full name no slash
      (if (not b-o-p)
          `(directory-file-name
            (nvp:buff--1 buffer-file-name ,no-default))
        `(if (directory-name-p ,b-o-p)
             (file-name-directory (file-truename ,b-o-p))
           (file-name-directory (file-truename ,b-o-p)))))

     ((eq type 'dn/)                    ;full name w/ slash
      (if (not b-o-p)
          `(nvp:buff--1 buffer-file-name ,no-default)
        `(if (directory-name-p ,b-o-p)
             (file-truename ,b-o-p)
           (file-name-directory (file-truename ,b-o-p)))))

     ((eq type 'dna)                    ;full name abbreviated
      (if (not b-o-p)
          `(abbreviate-file-name (nvp:buff--1 buffer-file-name ,no-default))
        `(abbreviate-file-name
          (if (directory-name-p ,b-o-p)
              (file-name-directory (file-truename ,b-o-p))
            (file-name-directory (file-truename ,b-o-p))))))

     ((eq type 'dna/)                    ;full name abbrev. w/ slash
      (if (not b-o-p)
          `(abbreviate-file-name (nvp:buff--1 buffer-file-name ,no-default))
        `(abbreviate-file-name
          (if (directory-name-p ,b-o-p)
              (file-truename ,b-o-p)
            (file-name-directory (file-truename ,b-o-p))))))
     
     ;; === Paths ===
     ((eq type 'fa) `(abbreviate-file-name ,b-o-p))
     ((eq type 'fs) `(file-name-nondirectory ,b-o-p))
     ((eq type 'fse) `(file-name-base (file-truename ,b-o-p)))
     ((eq type 'ext) `(file-name-extension (nvp:buff--2 ,b-o-p ,or-name)))
     ((eq type 'env)
      (if (not b-o-p)
          (user-error "No path supplied with `env' to `nvp:path'")
        (let ((res (substitute-env-in-file-name b-o-p)))
          (if (and res (file-exists-p res)) `,res
            `(substitute-env-in-file-name ,b-o-p)))))

     (t (user-error "%S unknown to `nvp:path'" type)))))


;; -------------------------------------------------------------------
;;; Syntax

(defmacro nvp:ppss (type &optional ppss point beg)
  "Return non-nil if syntax PPSS at POINT is of TYPE, one of the following.

`str'  -- Inside a string
`cmt'  -- Inside a comment
`soc'  -- Inside a string or comment

`partial' -- `parse-partial-sexp' from min or BEG to current pos or POINT"
  (let ((type (eval type)))
    (cond
     ;; ~~~ In comments/strings
     ((eq type 'str) `(nth 3 ,(or ppss `(syntax-ppss ,point))))
     ((eq type 'cmt) `(nth 4 ,(or ppss `(syntax-ppss ,point))))
     ((eq type 'soc)
      (macroexp-let2 nil syn (or ppss `(syntax-ppss ,point))
        `(or (car (setq ,syn (nthcdr 3 ,syn)))
             (car (setq ,syn (cdr ,syn)))
             (nth 3 ,syn))))
     
     ((eq type 'partial)
      (if ppss ppss
        `(parse-partial-sexp ,(or beg '(point-min)) ,(or point '(point)))))

     (t (user-error "%S unrecognized by `nvp:ppss'" type)))))

(defmacro nvp:if-ppss (type then &rest else)
  "Do THEN if syntax at point is of TYPE, otherwise ELSE."
  `(if (nvp:ppss ,type) ,then ,@else))

(defmacro nvp:unless-ppss (type &rest body)
  (declare (indent defun) (debug t))
  `(unless (nvp:ppss ,type)
     ,@body))


;; -------------------------------------------------------------------
;;; Scan lists

;; cc-defs `c-safe-scan-lists', paredit
(defmacro nvp:safe-scan-lists (from count depth &optional limit)
  "`scan-lists', but return nil instead of errors."
  (let ((res `(ignore-errors (scan-lists ,from ,count ,depth))))
    (if limit
        `(save-excursion
           (when ,limit
             ,(if (numberp count)
                  (if (< count 0)
                      `(narrow-to-region ,limit (point-max))
                    `(narrow-to-region (point-min) ,limit))
                `(if (< ,count 0)
                     (narrow-to-region ,limit (point-max))
                   (narrow-to-region (point-min) ,limit))))
           ,res)
      res)))

;; FIXME: use smie if available
(defmacro nvp:scan (type &optional pos limit)
  "Return position across various balanced expressions.
Start at POS if non-nil. Returns point at new position, or nil on failure.

`fl'  -- after forward balanced parens
`bl'  -- after backward list
`ful' -- up forward list
`bul' -- up backward list
`fdl' -- down forward list
`bdl' -- down list backward"
  (let ((type (eval type)))
    (cond
     ;; Return the point at different locations or nil
     ((eq type 'fl) `(nvp:safe-scan-lists ,(or pos '(point)) 1 0 ,limit))
     ((eq type 'bl) `(nvp:safe-scan-lists ,(or pos '(point)) -1 0 ,limit))
     ((eq type 'ful) `(nvp:safe-scan-lists ,(or pos '(point)) 1 1 ,limit))
     ((eq type 'bul) `(nvp:safe-scan-lists ,(or pos '(point)) -1 1 ,limit))
     ((eq type 'fdl) `(nvp:safe-scan-lists ,(or pos '(point)) 1 -1 ,limit))
     ((eq type 'bdl) `(nvp:safe-scan-lists ,(or pos '(point)) -1 -1 ,limit))

     (t (user-error "%S unrecognized by `nvp:scan'" type)))))


;; -------------------------------------------------------------------
;;; Positions

;;-- Syntactic whitespace

;; move backward across newline (\r or \r\n), returning non-nil if moved
(defsubst nvp--backward-nl ()
  (cond
   ((eq (char-before) ?\r)
    (backward-char)
    t)
   ((and (eq (char-before) ?\n)
         (eq (char-before (1- (point))) ?\r))
    (backward-char 2)
    t)
   (t nil)))

;; see `c-forward-comments'
(defsubst nvp--forward-sws (&optional escape)
  "Move past following whitespace and comments.
Line continuations, recognized by ESCAPE, are treated as whitespace as well."
  (and (integerp escape) (setq escape (char-to-string escape)))

  (let ((escaped-nl (concat escape "[\n\r]")))
    (while (or (forward-comment 5)
               (when (and escape (looking-at escaped-nl))
                 (forward-char (length escape))
                 t)))))

;; see `c-backward-comments'
(defsubst nvp--backward-sws (&optional escape)
  "Move backward across whitespace, comments, and line continuations."
  (and escape (stringp escape) (setq escape (string-to-char escape)))

  (let ((start (point)))
    (while (and (not (bobp))
                (if (let (moved-comment)
                      (while (and
                              (not (setq moved-comment (forward-comment -1)))
                              (nvp--backward-nl))) ; skip \r or \r\n
                      moved-comment)
                    ;; line continuations
                    (when (and escape
                               (looking-at "[\n\r]")
                               (eq (char-before) escape)
                               (< (point) start))
                      (backward-char)
                      t))))))

(defmacro nvp:skip-sws (direction &optional escape)
  "Skip `forward' or `backward' across comments, whitespace, and line
continuations."
  (and escape (stringp escape) (setq escape (string-to-char escape)))
  (let ((direction (eval direction)))
    (cond
     ((eq direction 'forward) `(nvp--forward-sws ,escape))
     ((eq direction 'backward) `(nvp--backward-sws ,escape))
     (t (error "Don't know how to skip %S" direction)))))

;;-- Whitespace only
(defmacro nvp:skip-ws-forward (escape &optional limit)
  "Skip horizontal/vertical whitespace and escaped newlines following point."
  (if limit
      `(let ((limit (or ,limit) (point-max)))
         (while (progn
		  ;; skip-syntax-* doesn't count \n as whitespace..
                  (skip-chars-forward " \t\n\r\f\v" limit)
                  (when (and (eq (char-after) ,escape)
                             (< (point) limit))
                    (forward-char)
                    (or (eolp)
                        (progn (backward-char) nil))))))
    `(while (progn
              (skip-chars-forward " \t\n\r\f\v")
              (when (and (eq (char-after) ,escape))
                (forward-char)
                (or (eolp)
                    (progn (backward-char) nil)))))))

(defmacro nvp:skip-ws-backward (escape &optional limit)
  "Skip over any whitespace, comments, and escaped nls preceding point."
  (if limit
      `(let ((limit (or ,limit (point-min))))
	 (while (progn
		  ;; skip-syntax-* doesn't count \n as whitespace..
		  (skip-chars-backward " \t\n\r\f\v" limit)
		  (and (eolp)
		       (eq (char-before) ?\\)
		       (> (point) limit)))
	   (backward-char)))
    `(while (progn
	      (skip-chars-backward " \t\n\r\f\v")
	      (and (eolp)
		   (eq (char-before) ,escape)))
       (backward-char))))

(defmacro nvp:skip-ws (direction &optional escape limit)
  "Skip horizontal/vertical whitespace and escaped nls in DIRECTION from point.
If ESCAPE is non-nil, treat as line continuation character (default '\\').
LIMIT restricts the search space when non-nil.
Direction is one of `forward', `backward'."
  (if escape
      (and (stringp escape) (setq escape (string-to-char escape)))
    (setq escape ?\\))

  (let ((direction (eval direction)))
    (if (eq direction 'forward)
        `(nvp:skip-ws-forward ,escape ,limit)
      `(nvp:skip-ws-backward ,escape ,limit))))

;;-- Points

(defmacro nvp:point--cse (&optional point &rest body)
  "Setup comments, save excursion and execute BODY."
  (declare (indent defun))
  `(save-excursion
     (comment-normalize-vars)
     ,@(if point `((goto-char ,point)))
     ,@body))

(defmacro nvp:point--se (&optional point &rest body)
  "Save excursion, possibly moving to POINT and executing BODY."
  (declare (indent defun))
  `(save-excursion
     ,@(if point `((goto-char ,point)))
     ,@body))

;; expanded version from cc-defs - comment nav, no c-specific commands,
;; different handling of ws/sws
;; ref: #<marker at 34444 in cc-defs.el.gz>
(defalias 'nvp:p 'nvp:point)
(defmacro nvp:point (position &optional point limit escape)
  "Return relative POSITION to POINT (default current point).
ESCAPE can be a string to match escaped newlines (default '\\').
LIMIT is passed to `scan-lists'.
POSITION can be one of the following symbols:

~~ Comments ~~
`cs'    -- start of current comment
`ce'    -- end of next comment
`csl'   -- start of comment on line
`cel'   -- end of comment on line

~~ Line positions ~~
`bol'   -- beginning of line
`boll'  -- beginning of logical line (skipping escaped NL)
`eol'   -- end of line
`eoll'  -- end of logical line (i.e. without escaped NL)
`bonl'  -- beginning of next line
`bonll' -- beginning of next logical line
`eonl'  -- end of next line
`bopl'  -- beginning of previous line
`eopl'  -- end of previous line

~~~ Sexps ~~~
`fl'    -- forward list
`bl'    -- backward list
`ful'   -- forward up list
`bul'   -- backward up list
`fdl'   -- forward down list
`bdl'   -- backward down list
`bod'   -- beginning of defun
`eod'   -- end of defun

~~~ Indentation ~~~
`ci'    -- current indentation
`boi'   -- beginning of indentation
`ionl'  -- indentation of next line
`iopl'  -- indentation of previous line

~~ Whitespace / Syntactic whitespace ~~
`bohws' -- beginning of horizontal whitespace (doesn't cross lines)
`eohws' -- end of horizontal whitespace
`bows'  -- beginning of whitespace (crossing lines)
`eows'  -- end of whitespace (crossing lines)
`bosws' -- beginning of syntactic whitespace (ws, comments, escaped nls)
`eosws' -- end of syntactic whitespace (ws, comments, escaped nls)

If the referenced position doesn't exist, the closest accessible point
to it is returned.  This function does not modify the point or the mark."
  (declare (debug t))
  (or escape (setq escape "\\\\"))
  (cl-assert (eq (car-safe position) 'quote) nil "Call with quoted 'position'")

  (let ((position (eval position)))
    (cond
     ;;=== Comments ====
     ((eq position 'cs)                 ;start of current comment
      `(nvp:point--cse ,point (comment-beginning)))
     
     ((eq position 'ce)                 ;end of next comment
      `(nvp:point--cse ,point
         (skip-syntax-forward " ")
         (if (looking-at-p comment-start-skip)
             (and (comment-forward) (point))
           (when-let ((beg (comment-beginning)))
             (goto-char beg)
             (and (comment-forward) (point))))))
     
     ((eq position 'cel)                ;end of comment on line
      `(nvp:point--cse ,point
         (beginning-of-line)
         (when-let ((beg (comment-search-forward (line-end-position) t)))
           (goto-char beg)
           (save-restriction
             (narrow-to-region (point) (line-end-position))
             (when (or (comment-forward)
                       (eolp)
                       (looking-at-p comment-end-skip))
               (point))))))
     
     ((eq position 'csl)                ;start of comment on line
      `(nvp:point--cse ,point
         (beginning-of-line)
         (comment-search-forward (line-end-position) t)))

     ;;=== Line positions ===
     ((eq position 'bol)                ;beginning of line
      (if (not point) '(line-beginning-position)
	`(nvp:point--se ,point
	   (beginning-of-line)
	   (point))))

     ((eq position 'boll)               ;beginning of logical line
      `(nvp:point--se ,point
         (while (progn
                  (beginning-of-line)
                  (unless (bobp)
                    (save-excursion
                      (forward-char -1)
                      (eq (logand 1 (skip-chars-backward ,escape)) 1))))
           (beginning-of-line 0))
         (point)))

     ((eq position 'eol)                ;end of line
      (if (not point) '(line-end-position)
	`(nvp:point--se ,point
	   (end-of-line)
	   (point))))

     ((eq position 'eoll)               ;end of logical line, w/o escaped NLs
      `(nvp:point--se ,point
	 (while (progn
		  (end-of-line)
		  (prog1 (eq (logand 1 (skip-chars-backward ,escape)) 1)))
	   (beginning-of-line 2))
	 (end-of-line)
	 (point)))

     ((eq position 'bopl)               ;beginning of previous line
      (if (not point) '(line-beginning-position 0)
	`(nvp:point--se ,point
	   (forward-line -1)
	   (point))))

     ((eq position 'bonl)               ;beginning of next line
      (if (not point) '(line-beginning-position 2)
	`(nvp:point--se ,point
	   (forward-line 1)
	   (point))))

     ((eq position 'bonll)              ;beginning of next logical line
      `(nvp:point--se ,point
         (while (progn
                  (end-of-line)
                  (prog1 (eq (logand 1 (skip-chars-backward "\\\\")) 1)))
           (beginning-of-line 2))
         (forward-line 1)
         (point)))

     ((eq position 'eopl)               ;end of previous line
      (if (not point) '(line-end-position 0)
	`(nvp:point--se ,point
	   (beginning-of-line)
	   (or (bobp) (backward-char))
	   (point))))

     ((eq position 'eonl)               ;end of next line
      (if (not point) '(line-end-position 2)
	`(nvp:point--se ,point
	   (forward-line 1)
	   (end-of-line)
	   (point))))

     ;;=== Indentation ===
     ((eq position 'ci)                 ;current indentation
      `(nvp:point--se ,point
         (back-to-indentation)
         (current-column)))
     
     ((eq position 'boi)                ;beginning of indentation
      `(nvp:point--se ,point
	 (back-to-indentation)
	 (point)))

     ((eq position 'iopl)               ;indent of previous line
      `(nvp:point--se ,point
	 (forward-line -1)
	 (back-to-indentation)
	 (point)))

     ((eq position 'ionl)               ;indent of next line
      `(nvp:point--se ,point
	 (forward-line 1)
	 (back-to-indentation)
	 (point)))

     ;;=== Sexps ===
     ((memq position '(fl bl ful bul fdl bdl)) ;scan lists
      `(nvp:scan ',position ,point ,limit))
     
     ((eq position 'bod)                ;beginning of defun
      `(nvp:point--se ,point
         (beginning-of-defun)
         (and defun-prompt-regexp
              (looking-at defun-prompt-regexp)
              (goto-char (match-end 0)))
	 (point)))

     ((eq position 'eod)                ;end of defun
      `(nvp:point--se ,point
         (end-of-defun)
	 (point)))

     ;;=== Whitespace / Syntactic whitespace ===
     ((eq position 'bohws)              ;beginning horizontal WS, same line
      `(nvp:point--se ,point
         (skip-syntax-backward " ")
	 (point)))

     ((eq position 'eohws)              ;end of horizontal WS, same line
      `(nvp:point--se ,point
         (skip-syntax-forward " ")
         (point)))

     ((eq position 'bows)               ;beginning WS across lines
      `(nvp:point--se ,point
         (nvp:skip-ws 'backward ,escape)
         (point)))

     ((eq position 'bosws)              ;beginning syntactic WS across lines
      `(nvp:point--se ,point
         (nvp:skip-sws 'backward ,escape)
         (point)))
     
     ((eq position 'eows)               ;end of WS across lines
      `(nvp:point--se ,point
         (nvp:skip-ws 'forward ,escape)
	 (point)))

     ((eq position 'eosws)              ;end of syntactic WS across lines
      `(nvp:point--se ,point
         (nvp:skip-ws 'forward ,escape)
         (point)))

     (t (error "Unknown buffer position requested: %s" position)))))

(defmacro nvp:goto (type &optional point limit escape)
  "Goto point returned from `nvp:point' (which see).
Returns nil if unsuccessful, point otherwise."
  `(ignore-errors (goto-char (nvp:point ,type ,point ,limit ,escape))))

;; paredit splicing reindent doesn't account for prompts
(defmacro nvp:preserving-column (&rest body)
  "Preserve point in column after executing BODY.
`paredit-preserving-column' doesn't properly account for minibuffer prompts."
  (declare (indent defun) (debug body))
  (let ((orig-indent (make-symbol "orig-indent"))
        (orig-col (make-symbol "orig-column")))
    `(let ((,orig-col (if (eq major-mode 'minibuffer-inactive-mode)
                          (- (current-column) (minibuffer-prompt-width))
                        (current-column)))
           (,orig-indent (nvp:point 'ci))
           (result (progn ,@body)))
       (let ((post-indent (nvp:point 'ci)))
         (goto-char
          (+ (nvp:point 'bol)
             (cond ((not (< ,orig-col ,orig-indent))
                    (+ ,orig-col (- post-indent ,orig-indent)))
                   ((<= post-indent ,orig-col) post-indent)
                   (t ,orig-col)))))
       result)))


;; -------------------------------------------------------------------
;;; Regions / things-at-point

(cl-defmacro nvp:tap-bounds (&optional thing &key pulse)
  "Return `bounds-of-thing-at-point' and pulse region unless NO-PULSE."
  (if (null pulse)
      `(bounds-of-thing-at-point ,(or thing ''symbol))
    `(progn
       (let ((bnds (bounds-of-thing-at-point ,(or thing ''symbol))))
         (when bnds
           (prog1 bnds
             (nvp-indicate-pulse-region-or-line (car bnds) (cdr bnds))))))))

(defmacro nvp:tap--i (form &optional prompt read-fn default hist)
  "Prompt interactively for input if running interactively and nothing found."
  (or prompt (setq prompt "Symbol: "))
  (if (eq (car-safe read-fn) 'quote) (setq read-fn (cdr read-fn)))
  `(unless noninteractive
     (or ,form
         ,(if read-fn `(,@read-fn ,prompt ,default ,hist)
            `(read-from-minibuffer ,prompt ,default nil nil ,hist ,default)))))

(cl-defmacro nvp:tap (type &optional tap beg end &key pulse hist)
  "Wrapper for bounds/contents of region/thing-at-points.
Things at point default to \\='symbols unless TAP is non-nil.
By regions of things at point are pulsed if PULSE is non-nil.
If BEG and END are non-nil, they are used as region bounds instead of those
listed below.

Trailing \"i\" indicates to prompt for input if nothing is found.

`tap'    -- Thing string, no props
`tapi'   --
`tapp'   -- Thing string w/ props
`tappi'  --
`btap'   -- Bounds of thing at point
`btapi'  --
`dwim'   -- If region is active, region string, otherwise thing-at-point
            (no props)
`dwimp'  -- Same, but with props
`bdwim'  -- Bounds of region or thing at point
`evar'   -- Elisp `variable-at-point'
`evari'  --
`evaru'  -- Elisp variable, but accept unbound as well
`evarui' --
`efunc'  -- Elisp `function-called-at-point'
`efunci' --
`tag'    -- Tag at point using `find-tag-default'"
  (let ((type (eval type)))
    (cond
     ((eq type 'tap) `(thing-at-point ,(or tap ''symbol) 'no-props))
     ((eq type 'tapi)
      `(nvp:tap--i (thing-at-point ,(or tap ''symbol) 'no-props) nil nil nil ,hist))
     ((eq type 'tapp) `(thing-at-point ,(or tap ''symbol)))
     ((eq type 'tappi)
      `(nvp:tap--i (thing-at-point ,(or tap ''symbol)) nil nil nil ,hist))
     ((eq type 'btap) `(nvp:tap-bounds ,tap :pulse ,pulse))
     ((eq type 'btapi)
      `(nvp:tap--i (nvp:tap-bounds ,tap :pulse ,pulse) nil nil nil ,hist))

     ;; DWIM: use regions if active, otherwise things-at-point
     ((eq type 'dwim)
      (if (and beg end) `(buffer-substring-no-properties ,beg ,end)
        `(if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (thing-at-point ,(or tap ''symbol) 'noprops))))
     ((eq type 'dwimp)
      (if (and beg end) `(buffer-substring ,beg ,end)
        `(if (use-region-p)
             (buffer-substring (region-beginning) (region-end))
           (thing-at-point ,(or tap ''symbol)))))
     ((eq type 'bdwim)
      (if (and beg end) `(cons ,beg ,end)
        `(if (use-region-p) (car (region-bounds))
           (nvp:tap-bounds ,(or tap ''symbol) :pulse ,pulse))))

     ;; elisp specific
     ((eq type 'evar) '(let ((var (variable-at-point)))
                         (and (symbolp var) var)))
     ((eq type 'evari)
      `(nvp:tap--i (let ((var (variable-at-point)))
                     (and (or (symbolp var) (/= 0 var)) var))
                   "Variable: " 'nvp-read-elisp-variable :none ,hist))
     ((eq type 'evaru) '(let ((var (variable-at-point 'any-symbol)))
                          (and (/= 0 var) var)))
     ((eq type 'evarui)
      `(nvp:tap--i (let ((var (variable-at-point 'any-symbol)))
                     (and (or (symbolp var) (/= 0 var)) var))
                   "Variable: " 'nvp-read-elisp-symbol :none ,hist))
     ((eq type 'efunc) '(function-called-at-point))
     ((eq type 'efunci)
      `(nvp:tap--i (function-called-at-point) "Function: "
                   'nvp-read-elisp-function :none ,hist))

     ;; tags
     ((eq type 'tag) '(find-tag-default))
     (t (user-error "Unknown `nvp:tap' type %S" type)))))

(cl-defmacro nvp:tap-or-region (type &optional tap &key pulse hist)
  "Return list of (beg end) of either active region or bounds of TAP."
  (declare (indent defun))
  `(when-let ((bnds (nvp:tap ,type ,tap nil nil :pulse ,pulse :hist ,hist)))
     (list (car bnds) (cdr bnds))))

(cl-defmacro nvp:with-region (beg end &optional thing &rest body
                                  &key pulse widen type &allow-other-keys)
  "Bind BEG END to dwim region bounds.
Uses region bounds if active, otherwise bounds of THING.
In WIDEN is non-nil, save restriction and widen before finding bounds."
  (declare (indent defun) (debug body))
  (nvp:skip-keywords body)
  (let ((bnds (make-symbol "bounds")))
    `(,@(if widen '(save-restriction (widen)) '(progn))
      (if-let* ((,bnds (nvp:tap ,(or type ''bdwim) ,(or thing ''paragraph)
                                nil nil :pulse ,pulse)))
          (cl-destructuring-bind (,beg . ,end) ,bnds
            ,@body)
        (user-error "nvp:with-region didn't find any bounds")))))


;; -------------------------------------------------------------------
;;; Save/Restore envs

(defmacro nvp:save-buffer-state (varlist &rest body)
  "Bind variables, `let*', in VARLIST and execute BODY.
State is then restored. See `c-save-buffer-state' and `save-buffer-state'."
  (declare (indent 1) (debug t))
  `(with-silent-modifications
     (let* ,varlist
       ,@body)))

(defmacro nvp:with-preserved-vars (vars &rest body)
  "Let bind VARS then execute BODY, so VARS maintain their original values.
VARS should be either a symbol or list or symbols."
  (declare (indent 1) (debug (form body)))
  `(cl-progv ,(if (listp vars) `,vars `(list ',vars)) nil
     (unwind-protect
         ,@body)))

;; from yasnippet #<marker at 126339 in yasnippet.el>
(defmacro nvp:letenv (env &rest body)
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

;;; Time

(defmacro nvp:file-older-than-days (file days)
  "non-nil if FILE last modification was more than DAYS ago."
  (declare (indent defun) (debug t))
  `(< (* 60 60 24 ,days)
      (time-to-seconds
       (time-subtract (current-time) (nth 5 (file-attributes ,file))))))

;;; Profile

;; modified from skeeto extras
(defmacro nvp:measure-time (times &rest body)
  "Rough measure of BODY run time, executing it TIMES and averaging results."
  (declare (indent defun))
  (garbage-collect)
  (let ((start (make-symbol "start"))
        (ts (make-symbol "times"))
        (avg (make-symbol "avg")))
    `(let ((,ts ,times)
           (,avg 0))
       (dotimes (_ ,ts)
         (let ((,start (float-time)))
           ,@body
           (cl-callf + ,avg (- (float-time) ,start))))
       (/ ,avg ,ts))))

(defmacro nvp:compare-runtimes (reps block1 block2)
  "Compare runtime averages of REPS for code BLOCK1 to BLOCK2."
  (declare (indent defun))
  (let ((avg1 (make-symbol "avg1"))
        (avg2 (make-symbol "avg2")))
    `(let ((,avg1 (nvp:measure-time ,reps ,block1))
           (,avg2 (nvp:measure-time ,reps ,block2)))
       (message "Reps(%d): (1) %g, (2) %g, ratio 1:2 => %g" ,reps ,avg1 ,avg2
                (/ ,avg1 ,avg2)))))

;; -------------------------------------------------------------------
;;; Toggled Tip

;; TODO:
;; - use help buffer with xref?
;; - better popup formatting
(cl-defmacro nvp:with-toggled-tip (popup
                                   &key
                                   (help-key "h") ;key-binding for help-fn
                                   (help-fn t)    ;more help (t is default)
                                   bindings       ;additional bindings
                                   (timeout 120)  ;pos-tip timeout
                                   keep           ;keep transient map
                                   use-gtk        ;use gtk tooltips
                                   (help-buffer '(help-buffer)))
  "Toggle POPUP, a help string, in pos-tip.
If HELP-FN is :none, HELP-KEY is not bound by default.
Normally, HELP-KEY triggers a function to jump to a full help description
related to the popup - hopefully in a buffer.
BINDINGS are an alist of (key . function) of additional keys to bind in the
transient keymap.
TIMEOUT is passed to `pos-tip-show-no-propertize'.
If USE-GTK is non-nil use gtk tooltips.
KEEP is passed to `set-transient-map'.
HELP-BUFFER is buffer with full help documentation. This is only applicable to
the default help function."
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
    `(let ((x-gtk-use-system-tooltips ,use-gtk))
       (unless (x-hide-tip)
         (pos-tip-show-no-propertize
          ,str nil nil nil ,timeout
          (pos-tip-tooltip-width (window-width) (frame-char-width)))
         (set-transient-map
          (let ((tmap (make-sparse-keymap)))
            (define-key tmap ,h-key ,h-fn)
            ,@(cl-loop for (k . b) in bindings
                       collect `(define-key tmap ,k ,b))
            tmap)
          ,keep
          ,exit-fn)))))


;; -------------------------------------------------------------------
;;; Wrapper functions

;;-- Wrapper functions to call mode-local values
(defmacro nvp:wrapper-function (symbol &optional doc)
  "Creates a simple wrapper function to call local SYMBOL function with
list of args. Optionally use DOC for function."
  (let ((fn (intern (string-remove-suffix "-function" (symbol-name symbol))))
        ;; (arg (make-symbol "arg"))
        )
    `(defun ,fn ()
       ,(or doc (format "Function to run local `%s'." symbol))
       (interactive)
       (setq prefix-arg current-prefix-arg)
       (call-interactively ,symbol))))

(defmacro nvp:wrapper-fuctions (&rest fun-docs)
  "Create list of wrapper functions.
FUN-DOCS is an alist of pairs of symbols with optional docs."
  (macroexp-progn
   (cl-loop for (sym . doc) in fun-docs
            collect `(nvp:wrapper-function ,sym ,doc))))


;; -------------------------------------------------------------------
;;; Caches / Hash

(defmacro nvp:hash-strings (strings &optional case-fold count)
  "Hash STRINGS.  If CASE-FOLD, hash is case-insensitive. If COUNT,
hash values are counts of strings, otherwise values are \\='t."
  `(progn
     ,@(if case-fold
           '((define-hash-table-test
              'case-fold
              (lambda (a b) (eq t (compare-strings a nil nil b nil nil t)))
              (lambda (a) (sxhash (upcase a)))
              ;; #'case-fold-string=
              ;; #'case-fold-string-hash
              )))
     (--> (make-hash-table :test ,(if case-fold ''case-fold '#'equal)
                           :size (length ,strings))
          (prog1 it
            (dolist (s ,strings)
              ,(if count
                   `(puthash s (1+ (gethash s it 0)) it)
                 `(puthash s t it)))))))

;;--- nvp-cache
(defmacro nvp:cache-get (key cache &rest body)
  "Get KEY from CACHE, or cache BODY result."
  (declare (indent 2))
  `(or (nvp-cache-get ,key ,cache)
       (cdr (setf (nvp-cache-get ,key ,cache) ,@body))))

;; Simple memoization / result caching
(cl-defmacro nvp:define-cache (func arglist &rest body
                                    &key local predicate cache
                                    &allow-other-keys)
  "Create a simple cache for FUNC results named FUNC or CACHE if non-nil.
Cache is either defvar (possibly local) so is updated when set to nil,
or PREDICATE is non-nil and returns nil."
  (declare (indent defun) (debug (sexp sexp sexp &form body)) (doc-string 3))
  (let ((docstring (when (stringp (car body)) (pop body))))
    (nvp:skip-keywords body)
    (let* ((fn (nvp:as-symbol func))
           (cache (or cache fn)))
      `(progn
         ,(if local `(defvar-local ,cache nil)
            `(defvar ,cache nil))
         (defun ,fn ,arglist
           ,docstring
           (or (,@(if predicate `(and ,predicate) '(progn)) ,cache)
               (setq ,cache (progn ,@body))))))))

(defmacro nvp:define-cache-runonce (func arglist &rest body)
  "Define cache function that will only compute cache once."
  (declare (indent defun) (debug defun) (doc-string 3))
  (let ((docstring (when (stringp (car body)) (pop body)))
        (cache (make-symbol "cache-runonce"))
        (fn (if (stringp func) (intern func) func)))
    `(defun ,fn ,arglist
       ,docstring
       (or (get ',fn ',cache)
           (let ((val (progn ,@body)))
             (prog1 val
               (put ',fn ',cache val)))))))

;;--- Lazy
(defmacro nvp:lazy-defvar (var fun)
  "When called the first time, VAR sets its value via FUN."
  (declare (indent 1) (debug (symbolp lambda-expr)))
  `(defvar ,var
     (lambda ()
       (when (functionp ,var)
         (setq ,var (funcall #',fun)))
       ,var)))

(defmacro nvp:lazy-val (var)
  "Get value of lazily defined VAR."
  `(if (functionp ,var) (funcall ,var) ,var))


;; -------------------------------------------------------------------
;;; Function building

;;-- Toggle
(cl-defmacro nvp:toggled-if (then &rest rest &key this-cmd &allow-other-keys)
  "Do THEN if `last-command' wasn't `this-command', otherwise do REST
and set `this-command' to nil so opposite happens next time."
  (declare (indent 1))
  (nvp:skip-keywords rest)
  `(if (not (eq this-command last-command))
       ,then
     (prog1 (progn ,@rest)
       (setq this-command ,(or this-cmd ''nvp--toggled-if)))))

(defmacro nvp:toggle-variable (variable)
  (declare (indent 1))
  `(progn
     (custom-load-symbol ',variable)
     (let ((set (or (get ',variable 'custom-set) 'set-default))
           (get (or (get ',variable 'custom-get) 'default-value)))
       (funcall set ',variable (not (funcall get ',variable))))))

;;-- Marks
(defmacro nvp:push-mark (cmd)
  "Push mark on first invocation of CMD."
  `(or (not (eq this-command ',cmd))
       (eq last-command ',cmd)
       (and transient-mark-mode mark-active)
       (push-mark)))

(defmacro nvp:mark-defun (&optional first-time &rest rest)
  "Mark blocks, expanding successively."
  `(let (deactivate-mark)
     (if (and (called-interactively-p 'any)
              (or (and (eq last-command this-command) (mark t))
                  (and transient-mark-mode mark-active)))
         (set-mark
          (save-excursion
            (goto-char (mark))
            ,@(or rest
                  (list
                   '(smie-forward-sexp 'halfsexp)
                   '(point)))))
       ,(or first-time '(nvp-mark-defun)))))

;; `cl-defmethod' for multiple modes
(cl-defmacro nvp:defmethod (method args &rest body &key modes &allow-other-keys)
  (declare (indent defun) (debug t))
  (nvp:skip-keywords body)
  (when (symbolp modes) (setq modes (eval modes)))
  (macroexp-progn
   (cl-loop for mode in modes
            collect `(cl-defmethod ,method
                       ,(append args (list '&context `(major-mode ,mode)))
                       ,@body))))

;; -------------------------------------------------------------------
;;; *Obsolete* Function generators
;; FIXME: most of these should either be generic or act on local variables
;; instead of being defined many times

;; switching between REPLs and source buffers -- maintain the name of the
;; source buffer as a property of the process running the REPL. Uses REPL-FIND-FN
;; if supplied to find/create the REPL buffer, REPL-LIVE-P is called to check
;; if it is alive (defaults to `buffer-live-p'
;; if REPL-HISTORY is non-nil `nvp-comint-add-history-sentinel' is added before the
;; buffers process-filter. REPL-INIT is called to create and return a new REPL
;; buffer. REPL-CONFIG is executed in the new REPL buffer after creation
(cl-defmacro nvp:repl-switch (name (&key repl-mode repl-buffer-name repl-find-fn
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
                 ,(or repl-process '(nvp:buffer-process)) :src-buffer)
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
             ,(and repl-config `(funcall ,repl-config))
             (process-put ,(or repl-process
                               '(get-buffer-process repl-buffer))
                          :src-buffer src-buffer)))))))


;; -------------------------------------------------------------------
;;; URL

;; FIXME: Asyncify
(defmacro nvp:with-url-buffer (url &rest body)
  "Do BODY in buffer with contents from URL."
  (declare (indent defun)
           (debug (sexp &rest form)))
  `(with-current-buffer (url-retrieve-synchronously ,url)
     ,@body))

(defmacro nvp:while-scanning-url (url regex &rest body)
  "Do BODY in buffer with URL contents at position of REGEX."
  (declare (indent defun)
           (debug (sexp sexp &rest form)))
  `(nvp:with-url-buffer ,url
     (goto-char (point-min))
     (while (re-search-forward ,regex nil t)
       ,@body)
     (kill-buffer)))


;; -------------------------------------------------------------------
;;; Advice

(defmacro nvp:advise-commands (advice where funcs &optional props)
  "Apply ADVICE to FUNCS at WHERE with PROPS.
FUNCS can be a list, quoted or not."
  (declare (indent defun))
  (setq funcs (nvp:list-unquote funcs))
  (macroexp-progn
   (cl-loop for func in funcs
            collect `(advice-add ',func ,where ,advice ,props))))

(defmacro nvp:unadvise-commands (advice funcs)
  "Remove ADVICE from FUNCS.
See `nvp:advise-commands'."
  (declare (indent defun) (debug t))
  (setq funcs (nvp:list-unquote funcs))
  (macroexp-progn
   (cl-loop for func in funcs
            collect `(advice-remove ',func ,advice))))

(defmacro nvp:remove-all-advice (funcs)
  "Remove all advice from list of FUNCS."
  (declare (indent 0))
  `(progn
     ,@(cl-loop for fn in funcs
                collect `(advice-mapc (lambda (advice _props) (advice-remove ',fn advice))
                                      ',fn))))

(defmacro nvp:eldoc-function (func &optional init)
  "Set local eldoc function."
  `(progn
     (add-function :before-until (local 'eldoc-documentation-function) #',func)
     ,(when init '(eldoc-mode))))

(defmacro nvp:run-once (symbol args &rest body)
  "Add advice to function SYMBOL (or multiple symbols if list). that runs
once. If SYMBOLs is a list, the advice only runs the first time any of the
symbols is evaluated. See `define-advice'."
  (declare (indent 2) (doc-string 3) (debug (sexp sexp def-body)))
  (let* ((where         (nth 0 args))
         (lambda-list   (nth 1 args))
         (name          (or (nth 2 args) (cl-gensym "nvp")))
         (depth         (nth 3 args))
         (props         (and depth `((depth . ,depth))))
         (symbols (nvp:as-list symbol))
         (advice (cond ((or (stringp name) (symbolp name))
                        (intern (format "%s@%s~once" (car symbols) name)))
                       (t (error "Unrecognized name spec `%S'" name)))))
    `(prog1
         (defalias ',advice
           (lambda ,lambda-list
             (declare-function ',advice "" ,lambda-list)
             (mapc (lambda (sym) (advice-remove sym ',advice)) ',symbols)
             ;; (advice-remove ',symbol ',advice)
             ,(if (functionp name)
                  `(funcall #',name ,@(nvp:arglist-args lambda-list))
                `(progn ,@body))))
       ,@(cl-loop for sym in symbols
                  collect
                  `(advice-add ',sym ,where #',advice
                               ,@(and props `(',props)))))))


;; -------------------------------------------------------------------
;;; Warn when required at runtime

;; FIXME: Doesn't work -- how to warn when required at runtime?
;; (put 'require 'byte-hunk-handler 'byte-compile-file-form-require)
(when (functionp 'backtrace-frames)
  (when (assoc '(t byte-compile-file-form-require
                   ((require 'nvp-macro))
                   nil)
               (backtrace-frames))
    (message "Warning: package 'nvp-macro required at runtime")))

(provide 'nvp-macro)
;;; nvp-macro.el ends here
