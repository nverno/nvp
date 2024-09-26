;;; nvp.el --- base configs -*- lexical-binding: t; -*-
;;
;; URL: https://github.com/nverno/nvp
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-local)
(require 'nvp-faces)
(require 'company)
(nvp:decls :p (projectile winner isearch consult lsp)
           :f (lsp-format-buffer projectile-regenerate-tags))
(nvp:auto "projectile" 'projectile-project-root)
(nvp:auto "pos-tip" 'pos-tip-show-no-propertize 'pos-tip-tooltip-width)
(nvp:package-define-root)

;;; Aliases
(defalias 'nvp-completing-read 'completing-read)
(put 'nvp-completing-read 'lisp-indent-function 'defun)
(put 'completing-read 'lisp-indent-function 'defun)
(defalias 'nvp-find-file-in-dir 'projectile-find-file-in-directory)
(defalias 'nvp-grab-symbol 'company-grab-symbol)

;;; Global vars
;; Note: most machine specific are compiled in init
(nvp:defvar nvp-program-search-paths
            (nvp:with-gnu/w32 `(,nvp/bin "~/.asdf/shims" "~/.local/bin" "/usr/local/bin")
              `(,nvp/bin ,nvp/binw))
            "Preferred search locations for executables.")

(defvar nvp-default-notes-files
  '("[Nn]otes.org" "[Tt][Oo][Dd][Oo].org" "[Rr][Ee][Aa][Dd][Mm][Ee].org")
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

(defvar nvp-tags-ctags-program (nvp:program "ctags") "Universal ctags.")

(defvar nvp-display-actions
  `( :buffer ((64 . ((nvp-display-buffer--maybe-in-frame)
                     (pop-up-frames . t)
                     (inhibit-switch-frame . nil)))
              (4 . ((display-buffer-same-window)
                    (inhibit-same-window  . nil)))
              (1 . ((nvp-display-buffer-other-window))))
     :file ((64 . find-file-other-frame)
            (4 . find-file)
            (1 . find-file-other-window))
     :ido ((4 . raise-frame)
           (1 . other-window))
     :find-func ((64 . find-function-other-frame)
                 (4 . find-function)
                 (1 . find-function-other-window))))

;;-- Local

(defvar-local nvp-local-abbrev-table nil "Abbrev table to use for mode.")
(nvp:defvar
  :local t :permanent t
  nvp-local-abbrev-file () "File containing local abbrev tables."
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

;; -------------------------------------------------------------------
;;; Functions

(nvp:auto "flycheck" 'flycheck-list-errors)
(nvp:decl projectile-test-project)

(defvar-local nvp-project-root-function #'nvp-project-locate-root)
(defvar-local nvp-mark-defun-function #'mark-defun)
(defvar-local nvp-fill-paragraph-function nil)
(defvar-local nvp-test-framework nil)

;;; Mode local hooks
;; use add-function, any reason to run hooks?
(nvp:defvar
  :local t :permanent t
  nvp-help-at-point-functions '(nvp-hap-local nvp-hap-company nvp-hap-info))

(eval-and-compile
  (defconst nvp-mode-hooks
    '( check-buffer format-buffer tag test compile debug install
       disassemble abbrev toggle run profile configure docs jump edit insert))

  (defconst nvp-mode-function-hooks
    (mapcar (lambda (type) (intern (concat "nvp-" (symbol-name type) "-functions")))
            nvp-mode-hooks))

  (defconst nvp-mode-default-functions
    (mapcar (lambda (el) (intern (format "nvp-%s-default-function" (symbol-name el))))
            nvp-mode-hooks)))

(eval-when-compile
  (defmacro nvp:define-function-hooks ()
    (macroexp-progn
     `(,@(mapcar (lambda (el) `(defvar-local ,el nil)) nvp-mode-function-hooks)
       ,@(mapcar (lambda (el) `(defvar-local ,el nil)) nvp-mode-default-functions)))))
(nvp:define-function-hooks)
(setq-default nvp-compile-default-function         #'nvp-compile-default)
(setq-default nvp-format-buffer-default-function
              (nvp:def nvp-format-buffer-default ()
                (interactive)
                (funcall (if (bound-and-true-p lsp-mode) #'lsp-format-buffer
                           #'delete-trailing-whitespace))))
(setq-default nvp-tag-default-function             #'projectile-regenerate-tags)
(setq-default nvp-install-default-function         #'projectile-install-project)
(setq-default nvp-check-buffer-default-function    #'flycheck-list-errors)
(setq-default nvp-test-default-function            #'projectile-test-project)
(setq-default nvp-configure-default-function       #'projectile-configure-project)
(setq-default nvp-run-default-function             #'projectile-run-project)
(setq-default nvp-abbrev-default-function          #'nvp-abbrevd)
(and (fboundp 'edit-indirect-region)
     (setq-default nvp-edit-default-function       #'edit-indirect-region))
(and (fboundp 'devdocs-lookup)
     (setq-default nvp-docs-default-function       #'devdocs-lookup))

(defsubst nvp-today () (format-time-string "%0m/%0e/%y"))


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
    (cond ((nth 3 syntax)
           (nvp-newline-dwim-string syntax arg))
          ((nth 4 syntax)
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
;;; Advices

;; add smooth-scrolling
(nvp:advise-commands #'do-smooth-scroll
  :after '(nvp-move-next5 nvp-move-prev5))

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

(declare-function xref--push-markers "xref")

(defun nvp@push-marker (orig-fn &rest args)
  "Push marker onto stack before calling ORIG-FN with ARGS."
  (let ((pushed
         (not (eq t (or ;; repeat-in-progress
                     (eq last-command this-command)
                     (let ((sym (and (subrp orig-fn)
                                     (intern (subr-name orig-fn)))))
                       (or (not (eq this-command sym))
                           (eq last-command sym)))
                     (xref--push-markers (current-buffer) (point)))))))
    (condition-case err
        (apply orig-fn args)
      (user-error (and pushed (xref-go-back))
                  (user-error (error-message-string err)))
      (error (and pushed (xref-go-back))
             (error (error-message-string err))))))

(nvp:advise-commands #'nvp@push-marker :around
  '( find-function find-variable find-function-on-key semantic-ia-fast-jump
     next-error previous-error
     flycheck-next-error flycheck-previous-error))

(defun nvp@push-mark (orig-fn &rest args)
  "Maybe push mark before call ORIG-FN with ARGS."
  (let* ((sym (cond ((subrp orig-fn)
                     (intern (subr-name orig-fn)))
                    ((symbolp orig-fn) orig-fn)
                    ((byte-code-function-p orig-fn) nil)
                    (t nil)))
         (pushed (not (eq t (nvp:push-mark sym)))))
    (condition-case err
        (apply orig-fn args)
      (user-error (and pushed (pop-mark))
                  (user-error (error-message-string err)))
      (error (and pushed (pop-mark))
             (error (error-message-string err))))))

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
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp.el ends here
