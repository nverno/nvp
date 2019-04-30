;;; nvp.el --- base configs -*- lexical-binding: t; -*-

;;; Commentary:

;; [![Build Status](https://travis-ci.org/nverno/nvp.svg?branch=master)](https://travis-ci.org/nverno/nvp)

;; - Global variables / variables set by modes
;; - Some general aliases that haven't made it to init
;; - Local site variables - compiled in init
;; - basic functions
;; - font faces

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(require 'nvp-local)
(nvp-decls)

;;; Aliases
(defalias 'nvp-completing-read 'ido-completing-read)
(put 'nvp-completing-read 'lisp-indent-function 'defun)
(defalias 'nvp-find-file-in-dir 'ido-find-file-in-dir)
(defalias 'nvp-grab-symbol 'company-grab-symbol)

;; root directory
(nvp-package-define-root :snippets t)

;; -------------------------------------------------------------------
;;; Variables

;;-- Global -- most machine specific are compiled in init
(nvp-defvar nvp-program-search-paths
  (nvp-with-gnu/w32 `(,nvp/bin "~/.asdf/shims" "~/.local/bin" "/usr/local/bin")
    `(,nvp/bin ,nvp/binw)))
(defvar nvp-default-org-file "gtd.org" "Default org file.")
(defvar nvp-window-configuration-stack () "Store window configurations.")
(defvar nvp-read-config-history () "Minibuffer jumping history.")
(defvar nvp-default-log-function #'nvp-log-default
  "Default logging function - called from process sentinels, etc.")
(defvar nvp-mode-font-additions () "Alist of mode font-lock additions.")

;;-- Local
;; Abbrevs
(defvar-local nvp-abbrev-local-file nil "File containing local abbrev tables.")
(put 'nvp-mode-header-regex 'permanent-local t)
(defvar-local nvp-abbrev-local-table nil "Abbrev table to use for mode.")
(defvar-local nvp-abbrev-dynamic-table nil "On-the-fly abbrev table.")
(defvar-local nvp-abbrev-prefix-chars ":<>=/#._[:alnum:]"
  "Default chars to include in abbrev prefixes")
(put 'nvp-abbrev-prefix-chars 'permanent-local t)

;; jumping variables -- might be set in dir-locals
(defvar-local nvp-local-notes-file () "Local notes/todo to jump dwim.")
(put 'nvp-local-notes-file 'permanent-local t)
(defvar-local nvp-local-books-directories () "Local book directory/s.")
(put 'nvp-local-notes-file 'permanent-local t)
(defvar-local nvp-local-uris () "Local URIs for webjumping.")
(put 'nvp-local-uris 'permanent-local t)
(defvar-local nvp-local-src-directories () "Local source dirs to jump.")
(put 'nvp-local-src-directories 'permanent-local t)
(defvar-local nvp-local-bookmark-file () "Local bookmarks to use.")
(put 'nvp-local-bookmark-file 'permanent-local t)

;;-- Possibly mode vars
(defvar-local nvp-mode-header-regex nil "Regex to move b/w headers.")
(defvar-local nvp-mode-snippet-dir nil "Mode's snippet directory.")
(defvar-local nvp-mode-install-targets () "Mode's external install targets.")

;; -------------------------------------------------------------------
;;; Functions

;;-- FIXME: Alists
(defvar nvp-help-at-point-functions ()
  "List of functions to return help at point.")
(defvar nvp-check-buffer-function #'checkdoc
  "List of functions to check buffer.")
(defvar nvp-disassemble-function #'disassemble)
(defvar nvp-test-function #'nvp-ert-run-tests
  "Functions called to run applicable tests in current context.")
(defvar nvp-tag-function () "Functions called to create tags.")
(defvar-local nvp-compile-function #'nvp-compile-default
  "Function to compile file.")
(defvar-local nvp-mark-defun-function #'mark-defun)

;; TODO:
;; - test/tag functions should call hooks
;; - not decided on check-buffer: this could do both buffer cleanup / linting
;;   either calling a hook or invoking a type of menu with options. Currently,
;;   it just invokes local indirect function.
;; - They could just be alists registering modes to functions?
(nvp-wrapper-fuctions
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

;; -------------------------------------------------------------------
;;; general read helpers

;; add default to prompt in non-nil
(defsubst nvp-prompt-default (prompt &optional default)
  (if default (format "%s (default %s): "
                      (substring prompt 0 (string-match "[ :]+\\'" prompt)) default)
    prompt))

;; Read with default of symbol-at-point. When COLLECTION is provided use
;; completing read, otherwise read from the minibuffer
(defun nvp-read-default (prompt &optional collection pred match initial
                                           hist default inherit)
  (when (and (not default) (setq default (thing-at-point 'symbol t)))
    (setq prompt (nvp-prompt-default prompt default)))
  (if collection
      (nvp-completing-read (nvp-prompt-default prompt default) collection pred
                           match initial hist default inherit)
    (read-from-minibuffer prompt nil nil nil nil default)))

;; -------------------------------------------------------------------
;;; Window configuration

;; save / restore window configurations
(defun nvp-window-configuration-save ()
  (interactive)
  (push (current-window-configuration) nvp-window-configuration-stack))

(defun nvp-window-configuration-restore (&rest _args)
  (interactive)
  (if-let* ((conf (pop nvp-window-configuration-stack)))
      (set-window-configuration conf)
    (if (> (length (window-list)) 1)
        (delete-window)
      (bury-buffer))))

;; -------------------------------------------------------------------
;;; Mode variables

;; return MODE value associated with KEY if exists
(define-inline nvp-mode-get-val (key &optional mode)
  (inline-letevals ((mode (or mode (quote major-mode))) key)
    (inline-quote (assq ,key (get ,mode 'nvp)))))

;; return mode value, default to cadr (first value minus the key)
(defsubst nvp-mode-val (key &optional all)
  (when-let* ((val (nvp-mode-get-val key)))
    (if all (cdr val)
      (cadr val))))

;; return KEY if defined otherwise lookup its mode value
(defsubst nvp-mode-local-or-val (key &optional all)
  (or (eval `(bound-and-true-p ,(intern-soft key)))
      (nvp-mode-val key all)))

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
(nvp-define-cache nvp-mode-header-regex ()
  "Get or create header regex based on comment syntax."
  :local t
  (let* ((comment (string-trim comment-start))
         (cs (regexp-quote comment))
         (multi (> (string-width comment) 1)))
    (if (not multi)
        ;; ignore things like ';;;###autoload'
        (format "^\\s-*%s%s\\(?:—\\|---\\|\*\\| |\\|%s\\)\\s-"
                cs cs cs)
      (format "^\\s-*%s\\(?:—\\|---\\|%s\\)\\s-" cs
              (regexp-quote (substring comment 1 2))))))

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

;; add additional newline when between syntactic open/closer
(defun nvp-newline-dwim--parens (&optional arg)
  (save-excursion
    (when (nvp-between-empty-parens-p)
      (newline-and-indent)))
  (newline arg 'interactive))

;; add a comment continuation string when in nestable doc comments
(defun nvp-newline-dwim--comment (&optional _comment-continue))

;; generics with defaults - lisp modes don't do anything special
(cl-defgeneric nvp-newline-dwim-prefix (&optional arg)
  "Generic function to handle newline dwim in special contexts."
  (newline arg 'interactive))

(cl-defgeneric nvp-newline-dwim-comment (&optional arg _comment-cont)
  "Generic function to handle newline dwim in comments."
  (newline arg 'interactive))

(cl-defgeneric nvp-newline-dwim-default (&optional arg)
  "Generic function to handle newline dwim syntactically."
  (let ((syntax (syntax-ppss)))
    (cond
     ((nvp-ppss 'str syntax)
      (newline arg 'interactive))
     ((nvp-ppss 'cmt syntax)
      (nvp-newline-dwim-comment arg))
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
(nvp-decl :pkg "paredit" paredit-move-past-close-and paredit-blink-paren-match
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
  (nvp-preserving-column
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
;;; IDO

;; XXX: switch over to counsel+ivy, at least for some stuff like
;; info-lookup, ag, other interfaces that look good
(defun nvp-ido-refresh-homedir ()
  "Refresh completion for homedir while ido is finding a file."
  (interactive)
  (ido-set-current-directory "~/")
  (setq ido-exit 'refresh)
  (exit-minibuffer))

(defun nvp-ido-yank ()
  "Forward to `yank'."
  (interactive)
  (if (file-exists-p (current-kill 0))
      (ido-fallback-command)
    (yank)))

(defun nvp-ido-backspace ()
  "Forward to `backward-delete-char'.
On error (read-only), quit without selecting."
  (interactive)
  (condition-case nil
      (backward-delete-char 1)
    (error
     (minibuffer-keyboard-quit))))

(defun nvp-ido-beginning-of-input ()
  (interactive)
  (goto-char (minibuffer-prompt-end)))

(defun nvp-ido-throw-dired ()
  (interactive)
  (throw 'dired t))

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
(defun nvp-mark-defun (arg &optional interactive-p)
  "Mark defun, skipping preceding comments."
  (interactive (list (prefix-numeric-value current-prefix-arg) 'interactive))
  (let ((skip-comments (not (region-active-p))))
    (setq prefix-arg (max 1 (/ (lsh arg -1) 4)))
    (funcall nvp-mark-defun-function prefix-arg)
    (and skip-comments (comment-forward (point-max)))
    (when interactive-p
      (nvp-use-transient-bindings
        (("c" . nvp-mark-expand-to-previous-comments))
        :repeat-key "h"))))

;; -------------------------------------------------------------------
;;; Company

(defun nvp-company-local (backends)
  "Make a buffer-local company backend."
  (set (make-local-variable 'company-backends)
       (delete-dups (cl-pushnew backends company-backends :test #'equal))))

;; -------------------------------------------------------------------
;;; Advices

;; add smooth-scrolling
(nvp-advise-commands 'do-smooth-scroll :after '(nvp-move-next5 nvp-move-prev5))

;; don't run `shell-mode-hook' during `shell-command' calls
(define-advice shell-command (:around (orig-fn &rest args) "no-hook")
  (let (shell-mode-hook)
    (apply orig-fn args)))

;; ensure spaces when aligning / commenting
(defun nvp--no-tabs (old-fn &rest args)
  (let (indent-tabs-mode)
    (apply old-fn args)))
(nvp-advise-commands 'nvp--no-tabs :around '(comment-dwim align align-regexp))

(provide 'nvp)
;;; nvp.el ends here
