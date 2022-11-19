;;; nvp.el --- base configs -*- lexical-binding: t; -*-
;;
;; URL: https://github.com/nverno/nvp
;;
;;; Commentary:
;; 
;; Library required at startup -- ~0.4 sec
;;
;; [![Build Status](https://travis-ci.org/nverno/nvp.svg?branch=master)](https://travis-ci.org/nverno/nvp)
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-local)
;; only external libraries required at startup: (actually in `emacs-startup-hook')
(require 'company)
(require 'smartparens)

(nvp:decls
 :f (winner-redo winner-undo isearch-repeat-forward isearch-repeat-backward consult-recent-file))

;;; Aliases
(defalias 'nvp-completing-read 'completing-read)
(put 'nvp-completing-read 'lisp-indent-function 'defun)
(defalias 'nvp-find-file-in-dir 'ido-find-file-in-dir)
(defalias 'nvp-grab-symbol 'company-grab-symbol)

;; autoloads
(nvp:auto "projectile" 'projectile-project-root)

;; root directory
(nvp:package-define-root)

;; -------------------------------------------------------------------
;;; My variables

;;-- Global -- most machine specific are compiled in init
(nvp:defvar nvp-program-search-paths
  (nvp:with-gnu/w32 `(,nvp/bin "~/.asdf/shims" "~/.local/bin" "/usr/local/bin")
    `(,nvp/bin ,nvp/binw))
  "Preferred search locations for executables.")

(defvar nvp-default-notes-files '("notes.org" "todo.org")
  "Default notes files.")

(defvar nvp-default-org-file "gtd.org"
  "Default org file.")

(defvar nvp-default-directories
  (list nvp/project nvp/class nvp/bin nvp/install nvp/work nvp/devel nvp/modes
        nvp/nvp nvp/site nvp/emacs nvp/build nvp/private nvp/scratch)
  "Interesting directories.")

(defvar nvp-window-configuration-stack ()
  "Store window configurations.")

(defvar nvp-default-log-function #'nvp-log-default
  "Default logging function - called from process sentinels, etc.")

(defvar nvp-mode-font-additions ()
  "Alist of mode font-lock additions.")

(defvar nvp-display-fallback-function #'nvp-display-fallback-dired
  "Fallback for unhandled prefix.")

(defvar nvp-fallback-minibuffer-function #'nvp-fallback-minibuffer-default
  "Function to call on exit from completing read.")

(defvar nvp-fallback-function nil
  "If non-nil, function to call after `nvp-fallback-command'. The first argument
is the result of `nvp-fallback-minibuffer-default' if called from minibuffer,
or nil.")

(defvar nvp-exit nil "Exit flag")

(defvar nvp-display-actions
  '(
    :buffer ((4 display-buffer-same-window
                ((inhibit-switch-frame . nil))
                ((inhibit-same-window  . nil)))
             (1 display-buffer-pop-up-window
                ((inhibit-same-window  . t))))
    :file ((4 find-file)
           (1 find-file-other-window))
    :ido ((4 raise-frame)
          (1 other-window))
    :find-func ((4 find-function)
                (1 find-function-other-window))))

;;-- Local
;; Abbrevs
(defvar-local nvp-abbrev-local-table nil "Abbrev table to use for mode.")
(defvar-local nvp-abbrev-dynamic-table nil "On-the-fly abbrev table.")
(nvp:defvar
  :local t :permanent t
  nvp-abbrev-local-file () "File containing local abbrev tables."
  nvp-abbrev-prefix-chars ":<>=/#._[:alnum:]"
  "Default chars to include in abbrev prefixes."
  ;; jumping variables -- might be set in dir-locals
  nvp-local-notes-file () "Local notes/todo to jump dwim."
  nvp-local-books-directories () "Local book directory/s."
  nvp-local-uris () "Local URIs for webjumping."
  nvp-local-src-directories () "Local source dirs to jump."
  nvp-local-bookmark-file () "Local bookmarks to use."
  nvp-local-compile-function () "Local compilation function."
  nvp-tabulated-list-select-action () "Function to call on list entry, when non-nil.")

;;-- Possibly mode vars
(defvar-local nvp-mode-header-regex nil "Regex to move b/w headers.")
(defvar-local nvp-mode-snippet-dir nil "Mode's snippet directory.")
(defvar-local nvp-mode-install-targets () "Mode's external install targets.")


;; -------------------------------------------------------------------
;;; Functions

;; use add-function, any reason to run hooks?
(defvar nvp-help-at-point-functions '(nvp-hap-company nvp-hap-info))
(defvar-local nvp-check-buffer-function #'checkdoc)
(defvar-local nvp-disassemble-function #'disassemble)
(defvar-local nvp-test-function #'nvp-ert-run-tests)
(defvar-local nvp-tag-function nil)
(defvar-local nvp-compile-function #'nvp-compile-default)
(defvar-local nvp-mark-defun-function #'mark-defun)
(defvar-local nvp-project-root-function #'projectile-project-root)
(defvar-local nvp-test-framework nil)
(defvar-local nvp-fill-paragraph-function nil)

;; TODO:
;; - test/tag functions should call hooks
;; - not decided on check-buffer: this could do both buffer cleanup / linting
;;   either calling a hook or invoking a type of menu with options. Currently,
;;   it just invokes local indirect function.
;; - They could just be alists registering modes to functions?
(nvp:wrapper-fuctions
 (nvp-check-buffer-function . nil)
 (nvp-test-function         . nil)
 (nvp-tag-function          . nil))


;; -------------------------------------------------------------------
;;; Faces

(defface nvp-highlight-face
  '((((class color) (background light))
     (:background "navy" :foreground "yellow" :weight bold :slant italic))
    (t (:background "yellow" :foreground "navy" :weight bold :slant italic)))
  "Really highlight stuff."
  :group 'nvp)

;; see cperl gaudy array/hash faces
(defface nvp-italic-variable-face
  `((((class grayscale) (background light))
     (:background "Gray90" :weight bold :slant italic))
    (((class grayscale) (background dark))
     (:foreground "Gray80" :weight bold :slant italic))
    (((class color) (background light))
     (:inherit font-lock-variable-name-face :weight bold :slant italic))
    (((class color) (background dark))
     (:inherit font-lock-variable-name-face :weight bold :slant italic))
    (t (:weight bold)))
  "Gaudy variable font locking - bold & italicized."
  :group 'nvp)

(defface nvp-italic-type-face
  `((((class grayscale) (background light))
     (:background "Gray90" :weight bold :slant italic))
    (((class grayscale) (background dark))
     (:foreground "Gray80" :weight bold :slant italic))
    (((class color) (background light))
     (:inherit font-lock-type-face :weight bold :slant italic))
    (((class color) (background dark))
     (:inherit font-lock-type-face :weight bold :slant italic))
    (t (:weight bold :slant italic)))
  "Gaudy type face - bold & italicized."
  :group 'nvp)

(defface nvp-info-face `((t (:inherit font-lock-variable-name-face :slant italic)))
  "Slanted info face."
  :group 'nvp)

(defface nvp-warning-face `((t (:inherit font-lock-keyword-face :slant italic)))
  "Slanted warning face."
  :group 'nvp)

(defface nvp-error-face `((t (:inherit font-lock-warning-face :slant italic)))
  "Slanted error face."
  :group 'nvp)

;; -------------------------------------------------------------------
;;; Company / Yasnippet

(with-eval-after-load 'yasnippet (require 'nvp-yas))

;; Note: this is set in compiled init -- but just in case
(make-variable-buffer-local 'company-backends)

(defun nvp-company-local (&rest backends)
  "Add backends to local `company-backends' (in a hook)."
  (dolist (b backends)
    (unless (member b company-backends)
      (push b company-backends))))


;; -------------------------------------------------------------------
;;; Movement

(defun nvp-move-next5 (&rest _ignored)
  (interactive)
  (forward-line 5))

(defun nvp-move-prev5 (&rest _ignored)
  (interactive)
  (forward-line -5))

(defun nvp-move-forward-defun ()
  (interactive)
  (or (not (eq this-command 'nvp-move-forward-defun))
      (eq last-command 'nvp-move-forward-defun)
      (and transient-mark-mode mark-active)
      (push-mark))
  (beginning-of-defun -1))

(defalias 'nvp-move-backward-defun 'beginning-of-defun)

;;--- Headings
;; these may vary by mode
;; Get or create header regex based on comment syntax.
(nvp:define-cache nvp-mode-header-regex ()
  :local t
  (nvp:heading-create-re))

(defun nvp-move-forward-heading (&optional back error)
  (interactive)
  (condition-case nil
      (progn
        (forward-line (if back -1 1))
        (if back (re-search-backward (nvp-mode-header-regex))
          (re-search-forward (nvp-mode-header-regex)))
        (forward-line 0))
    (error
     (forward-line (if back 1 -1))
     (user-error (format "No %s headings" (if back "previous" "more")))
     (and error (signal error t)))))

(defun nvp-move-previous-heading (&optional error)
  (interactive)
  (nvp-move-forward-heading 'back error))


;; -------------------------------------------------------------------
;;; Newline DWIM 

(defvar-local nvp-newline-comment-continue t
  "Use comment continuations in applicable modes.")

;; add additional newline when between syntactic open/closer
(defun nvp-newline-dwim--parens (&optional arg)
  (save-excursion
    (when (nvp:between-empty-parens-p)
      (newline-and-indent)))
  (newline arg 'interactive))

;; decide if newlines should add comment continuations in the
;; current comment block
(defun nvp-newline--comment-continue-p (syntax &optional cmt-cont)
  (nvp:defq cmt-cont comment-continue)
  (when cmt-cont
    (let ((cmt-beg-re (concat "\\s-*" (string-trim (regexp-quote cmt-cont)))))
      (save-excursion
        (beginning-of-line)
        (let ((start (<= (point) (nth 8 syntax))))
          (or (and start (goto-char (line-beginning-position 2))
                   (not (eq (get-char-property (point) 'face)
                            'font-lock-comment-face)))
              (looking-at-p cmt-beg-re)))))))

;; add a comment continuation string when in nestable doc comments
(defun nvp-newline-dwim--comment (syntax &optional arg cmt-cont)
  (if (not (and nvp-newline-comment-continue
                (or (integerp (nth 4 syntax))         ; nestable comments, eg. ocaml
                    (not (integerp (nth 7 syntax))))  ; /* */ style comments
                (nvp-newline--comment-continue-p syntax cmt-cont)))
      (newline-and-indent arg)
    (dotimes (_ (or arg 1))
      (insert ?\n (or cmt-cont comment-continue " "))
      (indent-according-to-mode))))

;; generics with defaults - lisp modes don't do anything special
(cl-defgeneric nvp-newline-dwim-prefix (&optional arg)
  "Generic function to handle newline dwim in special contexts."
  (newline arg 'interactive))

(cl-defgeneric nvp-newline-dwim-comment (_syntax arg)
  "Generic function to handle newline dwim in comments."
  (newline arg 'interactive))

(cl-defgeneric nvp-newline-dwim-string (_syntax arg)
  "Generic function to handle newline dwim in strings."
  (newline arg 'interactive))

(cl-defgeneric nvp-newline-dwim-default (&optional arg)
  "Generic function to handle newline dwim syntactically."
  (let ((syntax (parse-partial-sexp (point-min) (point))))
    (cond
     ((nvp:ppss 'str syntax)
      (nvp-newline-dwim-string syntax arg))
     ((nvp:ppss 'cmt syntax)
      (nvp-newline-dwim-comment syntax arg))
     ;; default to adding newline between paren delimiters
     (t (nvp-newline-dwim--parens arg)))))

(cl-defmethod nvp-newline-dwim-default
  (&context (major-mode emacs-lisp-mode) &optional arg _pairs)
  "Nothing special for lisp newlines."
  (newline arg 'interactive))

(defun nvp-newline-dwim (&optional arg)
  "Newline dwim.
Dispatches to generic handlers with ARG."
  (interactive "*P")
  (funcall-interactively 'nvp-newline-dwim-default arg))


;; -------------------------------------------------------------------
;;; Paredit 
(eval-when-compile (require 'paredit))
(nvp:decl paredit-move-past-close-and paredit-blink-paren-match
  paredit-indent-region paredit-splice-reindent)

(defun nvp-paredit-close-round ()
  "Close paren skipping over possible comments and call `expand-abbrev'."
  (interactive)
  (expand-abbrev)
  (let ((beg (point))                   ; keep comment on same line
        (end (paredit-move-past-close-and ?\) (lambda () (point)))))
    (unless (eq (line-number-at-pos) (line-number-at-pos beg))
      ;; we moved across lines -- skip back over comments / whitespace
      (forward-char -1)                 ; skip closer
      (forward-comment (- (point-max)))
      (when (not (eq (point) (1- end)))
        (insert-char ?\))
        (save-excursion
          (goto-char (1+ end))
          (delete-char -1)
          (delete-blank-lines))))
    (paredit-blink-paren-match nil)))

;; Problem: `paredit-splice-reindent' doesn't account for `minibuffer-prompt'
;; when `paredit' is active in the minibuffer, eg. during lisp evaluation
;; This redefinition accounts for the minibuffer width during reindent so the
;; point doesn't jump the width of the minibuffer after splicing
(defun nvp-paredit-splice-reindent (start end)
  (nvp:preserving-column
    ;; If we changed the first subform of the enclosing list, we must
    ;; reindent the whole enclosing list.
    (if (paredit-handle-sexp-errors
            (save-excursion
              (backward-up-list)
              (down-list)
              (paredit-ignore-sexp-errors (forward-sexp))
              (< start (point)))
          nil)
        (save-excursion (backward-up-list) (indent-sexp))
      (paredit-indent-region start end))))

(with-eval-after-load 'paredit
  ;; Redefine `paredit-splice-reindent'
  ;; this was a `defalias', but wasn't being loaded properly, I think it has
  ;; to do with this file being required in the init which is then compiled??
  (setf (symbol-function 'paredit-splice-reindent) #'nvp-paredit-splice-reindent))


;; -------------------------------------------------------------------
;;; Completion
(nvp:decl vertico-directory-tidy vertico-insert vertico-exit vertico--metadata-get)

(with-eval-after-load 'ido (require 'nvp-ido))

(defsubst nvp-vertico-completing-file-p ()
  (eq 'file (vertico--metadata-get 'category)))

(defun nvp-vertico-directory-up (&optional _)
  "Like `vertico-directory-up' except works when completing against
relative paths."
  (interactive)
  (when (and (> (point) (minibuffer-prompt-end))
             (eq (char-before) ?/)
             (nvp-vertico-completing-file-p))
    (let* ((path (buffer-substring (minibuffer-prompt-end) (point)))
           (parent (file-name-directory (directory-file-name path))))
      (delete-minibuffer-contents)
      (insert (or parent "")))
    t))

(defun nvp-vertico-expand-or-insert ()
  "When completing files, try expand to longest common prefix using partial
matching, otherwise just call `vertico-insert'. If this was previous
command, call `vertico-insert'. If there is only one match call
`vertico-exit'."
  (interactive)
  (--if-let (and (not (eq this-command last-command))
                 (nvp-vertico-completing-file-p)
                 (buffer-substring
                  (minibuffer-prompt-end)
                  (max (point) (minibuffer-prompt-end))))
      (let ((comp (completion-pcm-try-completion
                   it minibuffer-completion-table nil
                   (- (point) (minibuffer-prompt-end)))))
        (cond ((eq comp t) (vertico-exit))  ; only completion
              ((null comp) nil)             ; no match
              (t
               (pcase-let* ((`(,str . ,pt) comp)
                            (pos (+ pt (minibuffer-prompt-end))))
                 (cond ((string= it str)    ; no change
                        (goto-char pos))
                       (t                   ; replace with longest common prefix
                        (delete-minibuffer-contents)
                        (insert str)
                        (goto-char pos)))))))
    (vertico-insert)))

(defun nvp-fallback-minibuffer-default (&rest _)
  "Throw \\='nvp-fallback with input."
  (interactive)
  (let ((input (minibuffer-contents-no-properties)))
    ;; (nvp:unread input)
    (throw 'nvp-fallback input)))

(defun nvp-fallback-command (&rest args)
  "Set `nvp-exit' and call fallback functions."
  (interactive)
  (setq nvp-exit 'fallback)
  (and (minibufferp)
       (bound-and-true-p nvp-fallback-minibuffer-function)
       (funcall-interactively nvp-fallback-minibuffer-function args)))

(with-eval-after-load 'vertico-directory
  (setf (symbol-function 'vertico-directory-up) #'nvp-vertico-directory-up)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))


;; -------------------------------------------------------------------
;;; Repeat

(defvar nvp-repeat-key-enabled t)

(defun nvp-repeat-msg (repeat-key &optional bindings)
  (concat
   (format "[%S] repeat" repeat-key)
   (and bindings ", ")
   (mapconcat (lambda (b)
                (if-let* ((msg (plist-get b :msg)))
                    (format "[%S] %s" (car b) msg)
                  (pcase (cadr b)
                    (`(,(or 'function 'quote) ,sym)
                     (if (symbolp sym)
                         (format "[%S] %S" (car b) sym)
                       (format "[%S]" (car b))))
                    (_ (format "[%S]" (car b))))))
              bindings ", ")))

;; enable transient map for calling command
;; defaults to last basic char (no caps)
(defun nvp-repeat-command (&optional key no-indicator bindings &rest args)
  (when (and nvp-repeat-key-enabled
             (null overriding-terminal-local-map)
             (not (memq this-command `(nvp-repeat-command ,last-command))))
    (let* ((repeat-key (or key (nvp:input 'lbi)))
           (repeat-key-str (single-key-description repeat-key)))
      (when repeat-key
        (unless no-indicator (nvp-indicate-cursor-pre))
        (set-transient-map
         (let ((map (make-sparse-keymap)))
           (pcase-dolist (`(,k ,b) bindings)
             (define-key map (kbd k) (eval b)))
           (define-key map (vector repeat-key)
             (if args
                 `(lambda () (interactive)
                    (setq this-command ',this-command)
                    (funcall-interactively #',this-command ,@args))
               this-command))
           map)
         t
         (unless no-indicator
           (lambda () (nvp-indicate-cursor-post)
             (setq prefix-arg nil))))
        (or (minibufferp) (message (nvp-repeat-msg repeat-key-str bindings)))))))

(put 'nvp-repeat-command 'lisp-indent-function 'defun)

;; -------------------------------------------------------------------
;;; Marks

;; FIXME:
;; currently calls beginning of function twice since I was lazy to write a
;; function that would skip back over the previous comments. The seemingly
;; obvious `forward-comment' and other comment skipping functions all seem to
;; skip over both comments + whitespace which isn't what this should do.
(defun nvp-mark-expand-to-previous-comments ()
  "Move back previous comment block and end at beginning of line."
  (interactive)
  (forward-line 1)
  (beginning-of-defun-comments))

;; FIXME: on repeats the point moves back one line for some reason.
(defun nvp-mark-defun (&optional arg interactive)
  "Mark defun, skipping preceding comments."
  (interactive (list (prefix-numeric-value current-prefix-arg) 'interactive))
  (let ((skip-comments (not (region-active-p))))
    ;; (when interactive
    ;;   (setq prefix-arg (max 1 (/ (lsh arg -1) 4))))
    (funcall nvp-mark-defun-function arg)
    (and skip-comments (comment-forward (point-max)))
    (when interactive
      (nvp-repeat-command nil nil
        '(("c" #'nvp-mark-expand-to-previous-comments))))))


;; -------------------------------------------------------------------
;;; Advices

;; add smooth-scrolling
(nvp:advise-commands #'do-smooth-scroll :after '(nvp-move-next5 nvp-move-prev5))

;; don't run my `shell-mode-hook' during `shell-command' calls
(defvar shell-mode-hook)
(defun nvp@shell-command-no-hook (orig-fn &rest args)
  (let ((shell-mode-hook
         (delq 'nvp-shell-mode-hook (bound-and-true-p shell-mode-hook))))
    (apply orig-fn args)))

;; when running in batch mode `shell-mode-hook' is undefined
(unless noninteractive
  (advice-add 'shell-command :around #'nvp@shell-command-no-hook))

;; dont move point
(defun nvp@save-excurison (orig-fn &rest args)
  (save-excursion (apply orig-fn args)))

;; ensure spaces when aligning / commenting
(defun nvp@no-tabs (old-fn &rest args)
  (let (indent-tabs-mode)
    (apply old-fn args)))
(nvp:advise-commands #'nvp@no-tabs :around '(comment-dwim align align-regexp))

;; apply function in calling buffer when currently in minibuffer
(defun nvp@do-switch-buffer (old-fn &rest args)
  (with-current-buffer (let ((win (minibuffer-selected-window)))
                         (if (window-live-p win) (window-buffer win)
                           (current-buffer)))
    (apply old-fn args)))

;; use ido-completing-read
(defun nvp@read-with-ido (old-fn &rest args)
  (nvp:with-letf 'completing-read 'ido-completing-read
    (apply old-fn args)))

;; use ido-completion when reading environment variables interactively
;; (nvp:advise-commands #'nvp@read-with-ido
;;   :around '(read-envvar-name bookmark-jump))

;; after advice: repeat command with last basic input, or install transient MAP
(defun nvp@repeat (&optional map &rest args)
  (set-transient-map
   (or map
       (let ((km (make-sparse-keymap)))
         (define-key km (vector (nvp:input 'lbi))
           `(lambda ()
              (interactive)
              (apply #',this-command ,args)))
         km))
   t))

(nvp:bindings nvp-winner-map 'winner
  :create t :repeat t :indicate t
  ("p" . winner-undo)
  ("n" . winner-redo))

(nvp:bindings nvp-isearch-fast-map 'isearch
  :create t :repeat t :wrap t :indicate t
  ("]" . isearch-repeat-forward)
  ("[" . isearch-repeat-backward))

(nvp:bindings isearch-mode-map 'isearch
  ("C-s" . nvp/isearch-repeat-forward)
  ("C-r" . nvp/isearch-repeat-backward))


;; -------------------------------------------------------------------
;;; Bindings

;; don't require use-package
(defun nvp-autoload-keymap (keymap-symbol package &optional binding-map)
  "Autoload KEYMAP-SYMBOL from PACKAGE, binding calling keys as prefix
in `global-map' or BINDING-MAP if non-nil."
  (require package)
  (if (and (boundp keymap-symbol)
           (keymapp (symbol-value keymap-symbol)))
      (let* ((kv (this-command-keys-vector))
             (key (key-description kv))
             (keymap (symbol-value keymap-symbol)))
        (define-key (or binding-map global-map) (kbd key) keymap)
        (setq unread-command-events
              (--map (cons t it) (listify-key-sequence kv))))
    (error (format "package.el %s failed to define keymap %s"
                   package keymap-symbol))))


;; -------------------------------------------------------------------
;;; Windows / Buffers

;; save / restore window configurations
(defun nvp-window-configuration-save ()
  (push (current-window-configuration) nvp-window-configuration-stack))

(defun nvp-window-configuration-restore (&rest _args)
  (if-let* ((conf (pop nvp-window-configuration-stack)))
      (set-window-configuration conf)
    (if (> (length (window-list)) 1)
        (delete-window)
      (bury-buffer))))

;; weird this isn't builtin somewhere -- `kill-this-buffer' has issues
(defun nvp-kill-this-buffer ()
  (interactive)
  (nvp:ktb))

(provide 'nvp)
;;; nvp.el ends here
