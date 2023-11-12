;;; nvp.el --- base configs -*- lexical-binding: t; -*-
;;
;; URL: https://github.com/nverno/nvp
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-local)
;; only external libraries required at startup: (actually in `emacs-startup-hook')
(require 'company)
(require 'smartparens)

(nvp:decls
 :f ( winner-redo winner-undo
      isearch-repeat-forward isearch-repeat-backward
      consult-recent-file
      lsp-format-buffer
      ggtags-find-tag-dwim))

;;; Aliases
(defalias 'nvp-completing-read 'completing-read)
(put 'nvp-completing-read 'lisp-indent-function 'defun)
(defalias 'nvp-find-file-in-dir 'projectile-find-file-in-directory)
(defalias 'nvp-grab-symbol 'company-grab-symbol)

;; autoloads
(nvp:auto "projectile" 'projectile-project-root)
(nvp:auto "pos-tip" 'pos-tip-show-no-propertize 'pos-tip-tooltip-width)

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
  (list nvp/project nvp/class nvp/bin nvp/work nvp/devel nvp/scratch)
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
  "Function to call after `nvp-fallback-command'.
The first argument is the result of `nvp-fallback-minibuffer-default' if
called from minibuffer, or nil.")

(defvar nvp-exit nil "Exit flag.")

(defvar nvp-display-actions
  '( :buffer ((4 display-buffer-same-window
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
(defvar-local nvp-mode-name nil "Mode to use instead of `major-mode'.")
(defvar-local nvp-mode-snippet-dir nil "Mode's snippet directory.")
(defvar-local nvp-mode-install-targets () "Mode's external install targets.")


;; -------------------------------------------------------------------
;;; Functions

(nvp:auto "flycheck" 'flycheck-list-errors)
(nvp:decl projectile-test-project)

(defvar-local nvp-project-root-function #'projectile-project-root)
(defvar-local nvp-mark-defun-function #'mark-defun)
(defvar-local nvp-fill-paragraph-function nil)
(defvar-local nvp-test-framework nil)

;;; Mode local hooks
;; use add-function, any reason to run hooks?
(nvp:defvar :local t :permanent t
            nvp-help-at-point-functions '(nvp-hap-company nvp-hap-info))

(defconst nvp-mode-function-hooks
  (eval-when-compile 
    (cl-loop for type in '( check-buffer format-buffer tag test compile debug
                            disassemble abbrev toggle run profile docs)
             for name = (intern (concat "nvp-" (symbol-name type) "-functions"))
             collect name))
  "Mode local function hooks.")

(eval-when-compile
  (defmacro nvp:define-function-hooks ()
    (macroexp-progn
     `(,@(cl-loop
          for h in nvp-mode-function-hooks
          for def = (intern (replace-regexp-in-string
                             "\\(.*\\)-functions" "\\1-default-function"
                             (symbol-name h)))
          nconc `((defvar-local ,h nil)
                  (defvar-local ,def nil)))))))
(nvp:define-function-hooks)
(setq-default nvp-compile-default-function #'nvp-compile-default)
(setq-default nvp-tag-default-function #'ggtags-find-tag-dwim)
(setq-default nvp-format-buffer-default-function #'lsp-format-buffer)
(setq-default nvp-check-buffer-default-function #'flycheck-list-errors)
(setq-default nvp-test-default-function #'projectile-test-project)


;; -------------------------------------------------------------------
;;; Faces

(defface nvp-block-face
  '((((class color) (background light))
     (:background "#f2e5bc" "#ffffd7" :inherit highlight))
    (t (:background "#32302f" "#303030" :inherit highlight)))
  "Face for special blocks."
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

(defface nvp-line-escape-face
  '((t ( :inherit font-lock-misc-punctuation-face
         :weight bold :foreground "hot pink" :override t)))
  "Line escape face."
  :group 'nvp)

;; -------------------------------------------------------------------
;;; Company / Yasnippet

(with-eval-after-load 'yasnippet (require 'nvp-yas))

;; Note: this is set in compiled init -- but just in case
(make-variable-buffer-local 'company-backends)

(defun nvp-company-local (&rest backends)
  "Add BACKENDS to local `company-backends' (in a hook)."
  (dolist (b backends)
    (unless (member b company-backends)
      (push b company-backends))))


;; -------------------------------------------------------------------
;;; Movement

(defun nvp-move-next5 (&rest _ignored)
  "Move 5 lines forward."
  (interactive)
  (forward-line 5))

(defun nvp-move-prev5 (&rest _ignored)
  "Move 5 lines previous."
  (interactive)
  (forward-line -5))

(defun nvp-move-forward-defun ()
  "Move forward defun."
  (interactive)
  (nvp:push-mark nvp-move-forward-defun)
  (beginning-of-defun -1))

;; wrap here so when `beginning-of-defun' is remapped, like in treesit, it
;; doesn't mess up bindings/repeat maps
(defun nvp-move-previous-defun (&optional arg)
  "Move backward defun."
  (interactive "P")
  (nvp:push-mark nvp-move-previous-defun)
  (beginning-of-defun arg))

;;--- Headings
;; these may vary by mode
;; Get or create header regex based on comment syntax.
(nvp:define-cache nvp-mode-header-regex ()
  :local t
  (nvp:heading-create-re))

(defun nvp-move-forward-heading (&optional back error)
  "Move forward heading.
Move backward if BACK. If ERROR throw error when no more headings."
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
  "Move backward headings.
If ERROR, throw error when no more headings."
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
  "Generic function to handle newline dwim in special contexts with prefix ARG."
  (newline arg 'interactive))

(cl-defgeneric nvp-newline-dwim-comment (_syntax arg)
  "Generic function to handle newline dwim in comments with prefix ARG."
  (newline arg 'interactive))

(cl-defgeneric nvp-newline-dwim-string (_syntax arg)
  "Generic function to handle newline dwim in strings with prefix ARG."
  (newline arg 'interactive))

(cl-defgeneric nvp-newline-dwim-default (&optional arg)
  "Generic function to handle newline dwim syntactically with prefix ARG."
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
  "Nothing special for MAJOR-MODE Lisp newlines (prefix ARG)."
  (newline arg 'interactive))

(defun nvp-newline-dwim (&optional arg)
  "Newline dwim.
Dispatches to generic handlers with ARG."
  (interactive "*P")
  (funcall-interactively 'nvp-newline-dwim-default arg))


;; -------------------------------------------------------------------
;;; Paredit
(eval-when-compile (require 'paredit))
(nvp:decl paredit-move-past-close-and paredit-blink-paren-match)

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


;; -------------------------------------------------------------------
;;; Completion
(nvp:decl vertico-directory-tidy vertico-insert vertico-exit vertico--metadata-get)

(defsubst nvp-vertico-completing-file-p ()
  (eq 'file (vertico--metadata-get 'category)))

(defun nvp-vertico-directory-up (&optional _)
  "Like `vertico-directory-up' except works when completing against relative paths."
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
  "When completing files, try expand to LCP using partial matching.
Otherwise just call `vertico-insert'. If this was previous command, call
`vertico-insert'. If there is only one match call `vertico-exit'."
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
  "Set `nvp-exit' and call fallback functions with ARGS."
  (interactive)
  (setq nvp-exit 'fallback)
  (and (minibufferp)
       (bound-and-true-p nvp-fallback-minibuffer-function)
       (funcall-interactively nvp-fallback-minibuffer-function args)))

;;; FIXME: fix this
(with-eval-after-load 'vertico-directory
  (setf (symbol-function 'vertico-directory-up) #'nvp-vertico-directory-up)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

;; -------------------------------------------------------------------
;;; Marks

(defvar-keymap nvp-repeat-mark-defun-map
  :repeat (:enter (nvp-mark-defun))
  "c" #'nvp-mark-expand-to-previous-comments
  "h" #'nvp-mark-defun)

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
(defun nvp-mark-defun (&optional arg)
  "Call mark defun with ARG, skipping preceding comments."
  (interactive (list (prefix-numeric-value current-prefix-arg)))
  (let ((skip-comments (not (region-active-p))))
    ;; (when interactive
    ;;   (setq prefix-arg (max 1 (/ (lsh arg -1) 4))))
    (funcall nvp-mark-defun-function arg)
    (and skip-comments (comment-forward (point-max)))))


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
  (dolist (cmd '(shell-command async-shell-command))
    (advice-add cmd :around #'nvp@shell-command-no-hook)))

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


;; -------------------------------------------------------------------
;;; Bindings

;; don't require use-package
(defun nvp-autoload-keymap (keymap-symbol package &optional binding-map)
  "Autoload KEYMAP-SYMBOL from PACKAGE.
Binds calling keys as prefix in `global-map' or BINDING-MAP if non-nil."
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

(defun nvp-kill-this-buffer ()
  "Just kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(provide 'nvp)
;;; nvp.el ends here
