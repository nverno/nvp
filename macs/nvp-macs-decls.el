;;; nvp-macs-decls.el --- silence compiler -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(require 'nvp-macs-common "macs/nvp-macs-common")

(defun nvp:-decl-function-p (&optional pos)
  "Determine if POS, or point, is at beginning of function.
Note: simple check looks for preceding \"(\" or \"#'\"."
  (if (eq ?\( (char-before pos)) (not (eq ?\( (char-before (1- pos))))
    (and (eq ?\' (char-before pos))
         (eq ?\# (char-before (1- (or pos (point))))))))

(defun nvp:-decl-prefix (prefix &optional ignore)
  "Gather functions/variables with matching PKG prefix in current buffer.
If IGNORE is non-nil, exclude those matching regexp IGNORE.
Note: "
  (let ((file (nvp:load-file-name))
        fns vars)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents file)
          (emacs-lisp-mode)                ; string/comment syntax
          (goto-char (point-min))
          (while (re-search-forward
                  (concat "\\(\\_<" prefix "[/@_:-][^ \n\t]+\\_>\\)") nil t)
            (unless (or (let* ((syn (syntax-ppss nil)))
                          (or (car (setq syn (nthcdr 3 syn)))
                              (car (setq syn (cdr syn))) (nth 3 syn)))
                        (and ignore
                             (save-excursion
                               (goto-char (match-beginning 0))
                               (looking-at-p ignore))))
              (ignore-errors
                (push (intern (buffer-substring-no-properties
                               (match-beginning 1) (match-end 1)))
                      (if (nvp:-decl-function-p (match-beginning 0))
                          fns
                        vars))))))
      (error (error "nvp:-decl-prefix: %S" (error-message-string err))))
    (list :f (cl-remove-duplicates fns)
          :v (cl-remove-duplicates vars))))

(cl-defmacro nvp:decl-prefix (prefix &key ignore)
  "Add declares for functions/variables with matching PREFIX.
If IGNORE is non-nil, exclude those matching regexp IGNORE."
  (declare (debug t))
  (cl-destructuring-bind (&key f v)
      (nvp:-decl-prefix (nvp:as-string prefix) (and ignore (nvp:as-string ignore)))
    `(progn
       ,(when f `(nvp:decl ,@f))
       ,(when v `(nvp:local-defvars ,@v)))))

(cl-defmacro nvp:decl-prefixes (&rest decls)
  (macroexp-progn
   (cl-loop for d in decls
            collect
            `(nvp:decl-prefix
              ,(if (plistp d) (plist-get d :prefix) d)
              :ignore ,(and (plistp d) (plist-get d :ignore))))))

(defmacro nvp:local-defvars (&rest defvars)
  (macroexp-progn
   (cl-loop for dv in defvars
            collect `(defvar ,dv))))

(defmacro nvp:local-vars ()
  `(nvp:local-defvars
    nvp/abbrevs
    nvp/auto
    nvp/auto-site
    nvp/devel
    nvp/site
    nvp/modes
    nvp/emacs
    nvp/build
    nvp/project
    nvp/info
    nvp/bin
    nvp/binw
    nvp/msys
    nvp/cygwin
    nvp/vms
    nvp/git
    nvp/test
    nvp/lisp
    nvp/config
    nvp/custom
    nvp/data
    nvp/template
    nvp/snippet
    nvp/scratch
    nvp/class
    nvp/work
    nvp/bookmark
    nvp/cache
    nvp/backup
    nvp/org
    nvp/books
    nvp/install
    nvp/private
    ;; my vars
    nvp-mode-name
    nvp-mode-cache
    nvp-mode-default-functions
    nvp-mode-function-hooks
    nvp-mode-hooks
    nvp-mode-header-regex
    nvp-mode-snippet-dir
    nvp-mode-font-additions
    nvp-tags-ctags-program
    nvp-debug-breakpoint-string
    nvp-local-compile-function
    nvp-local-notes-file
    nvp-local-books-directories
    nvp-local-uris
    nvp-local-src-directories
    nvp-local-bookmark-file
    nvp-tabulated-list-select-action
    nvp-default-log-function
    nvp-default-org-file
    nvp-window-configuration-stack
    nvp-program-search-paths
    nvp-abbrevd-obarray nvp-local-abbrev-file nvp-local-abbrev-table
    nvp-abbrev-prefix-chars
    nvp-repl-alist
    nvp-repl-current
    nvp-repl-default
    nvp-repl-find-functions
    nvp-help-at-point-functions
    nvp-install-default-function
    nvp-check-buffer-default-function
    nvp-configure-default-function
    nvp-disassemble-default-function
    nvp-debug-default-function
    nvp-tag-default-function
    nvp-test-default-function
    nvp-format-buffer-default-function
    nvp-abbrev-default-function
    nvp-toggle-default-function
    nvp-run-default-function
    nvp-compile-default-function
    nvp-compile-default-function
    nvp-mark-defun-function
    ;; fallbacks
    nvp-exit
    nvp-fallback-function
    nvp-fallback-minibuffer-function
    ;; hippie expansion variables
    nvp-he-flex-matcher nvp-he-flex-prefix-to-re nvp-he-flex-prefix-from-re
    nvp-he-flex-symbol-beg nvp-he-case-fold-search
    ;; builtin hippie
    hippie-expand-try-functions-list hippie-expand-only-buffers
    he-search-string he-tried-table he-expand-list
    ;;=== emacs base ===
    package-user-dir
    recentf-list
    ielm-working-buffer ielm-dynamic-return
    org-babel-do-load-languages org-src-lang-modes
    local-abbrev-table
    bug-reference-bug-regexp bug-reference-url-format
    crm-separator
    compilation-error-regexp-alist compilation-error-regexp-alist-alist
    compilation-error-face compilation-warning-face compilation-info-face
    explicit-shell-file-name
    imenu-generic-expression imenu--index-alist imenu-auto-rescan
    repeat-in-progress
    ;; Tree-sitter, treesit
    treesit-font-lock-feature-list
    treesit-font-lock-settings
    ;; align
    align-to-tab-stop
    ;; ido
    ido-default-buffer-method ido-default-file-method
    ;;=== external pkg ===
    devdocs-current-docs
    zeal-at-point-docset
    dash-docs-docsets
    company-candidates company-selection
    ;; yasnippet
    yas-text yas-selected-text yas-wrap-around-region
    ;; ace-link
    ace-link-fallback-function
    ;; vertico
    vertico--input
    ;; url
    url-http-end-of-headers
    url-request-method url-request-extra-headers url-request-data
    ;; projectile
    projectile-tags-file-name
    projectile-tags-command
    projectile-tags-exclude-patterns))

(cl-defmacro nvp:decls (&key v f p)
  (declare (debug t))
  `(progn
     (nvp:local-vars)
     ,(when v
        `(nvp:local-defvars ,@v))
     ,(when f
        `(nvp:decl ,@f))
     ,(when p
        `(nvp:decl-prefixes ,@(nvp:as-list p)))
     (nvp:decl
       ;; nvp
       nvp-newline-dwim--comment
       nvp-completing-read
       nvp-find-file-in-dir
       ;; from compiled init
       nvp-autoload-keymap
       nvp-mode-header-regex
       nvp-mark-defun nvp-mark-expand-to-previous-comments
       nvp-company-local
       nvp-narrow-dwim
       nvp-paredit-close-round
       nvp-move-previous-heading nvp-move-forward-heading
       nvp-xref-go nvp-etags-find-definitions
       nvp-repeat-set-cursor nvp-repeat-abort
       nvp-indicate-abort nvp-indicate-cursor-pre nvp-indicate-cursor-post
       ;; hap
       nvp-help-at-point nvp-hap-elisp nvp-hap-info nvp-hap-man nvp-hap-lsp
       nvp-hap-company nvp-hap-semantic nvp-hap-treesit nvp-hap-local nvp-hap-word
       ;; util
       nvp-regex-map-across-matches
       ;; install / package
       nvp-pkg-directory-dwim
       ;; project
       nvp-project-root nvp-project-parent nvp-project-locate-root
       ;; nvp-cycle.el
       nvp-cycle
       ;; reading
       nvp-read-elisp-symbol
       nvp-read-elisp-function
       nvp-read-elisp-variable
       nvp-read-mode
       ;; modes
       nvp-scratch-minor-mode
       ;; logging
       nvp-results-title
       nvp-log-mode
       nvp-log
       nvp-log-default
       nvp-display-fallback-function
       nvp-display-fallback-dired
       nvp-display-location
       ;; windows
       nvp-window-configuration-restore
       nvp-window-configuration-save
       ;; hippie
       nvp-try-expand-history
       nvp-he-history-setup
       nvp-he-history-remove-trailing-paren
       nvp-try-expand-flex
       nvp-he-flex-lisp
       nvp-he-elisp-setup
       nvp-he-flex-lisp-setup
       nvp-he-flex-camel/snake
       nvp-he-chained-symbol-beg
       nvp-try-expand-dabbrev-closest-first
       nvp-try-expand-local-abbrevs
       ;; repls / shell
       nvp-comint-setup-history
       nvp-repl-add
       nvp-repl-jump
       nvp-repl-send-string
       nvp-repl-send-region
       nvp-repl-send-line
       nvp-repl-send-buffer
       nvp-repl-send-defun
       nvp-repl-cd
       nvp-repl-pwd
       nvp-repl-help
       nvp-repl-menu
       nvp-repl-config
       nvp-repl-setup-input-filter
       nvp-repl-eval-show-result
       nvp-repl-eval-string
       nvp-repl-eval-sexp
       nvp-repl-eval-result-value
       nvp-repl-eval-show-result
       nvp-repl-show-result
       nvp-shell
       nvp-shell-launch-terminal
       ;; indication
       nvp-indicate-pulse-region-or-line
       nvp-indicate-modeline
       ;; procs / inf
       nvp-proc-default-filter
       nvp-proc-default-sentinel
       nvp-async-shell-command-to-string
       nvp-inf-read-process
       ;; imenu
       nvp-imenu-setup
       nvp-imenu
       nvp-imenu-wrapper
       ;; toggle
       nvp-toggle-local-variable
       ;; abbrev
       nvp-abbrevd
       nvp-abbrev-from-splitters nvp-abbrev-from-words
       nvp-abbrev-write-abbrev-table
       nvp-abbrev-grab
       nvp-abbrev-expand-p
       nvp-abbrev-expand-not-after-punct-p
       nvp-abbrev-expand-after-symbols-hook
       nvp-abbrev-make-post-insert-hook
       nvp-grab-symbol
       ;; test
       nvp-ert-run-tests
       ;; compile
       nvp-compile
       nvp-compile-default
       nvp-cmake-compile
       ;; environment
       nvp-env-add
       nvp-env-path-add
       ;; cache
       nvp-cache-get
       nvp-cache-create
       ;; nvp-org
       nvp-org-links
       ;; theme
       nvp-theme-switch
       ;; setup
       nvp-setup-program nvp-setup-local
       ;; nvp-auto
       nvp-install-mode nvp-install-modes
       nvp-lookup-password
       ;; Tags
       nvp-tag-list-tags nvp-tag-list-decls nvp-tag-repo
       nvp-tag-find nvp-tag-find-etag
       ;; override
       nvp-buffer-local-set-key
       nvp-buffer-local-set-minor-mode-key)

     (nvp:decl                          ; builtins
       minibuffer-keyboard-quit
       ielm ielm-return
       ert-run-tests-interactively
       hs-already-hidden-p hs-show-all hs-show-block hs-hide-all hs-hide-block
       w32-shell-execute
       xref-pop-marker-stack xref-push-marker-stack
       org-comment-dwim
       he-substitute-string he-reset-string he-string-member he-init-string
       find-function-other-window
       imenu--make-index-alist imenu-default-create-index-function
       ;; treesit
       treesit-font-lock-rules
       treesit-font-lock-recompute-features)

     (nvp:decl                          ; external packages
       edit-indirect
       pos-tip-show pos-tip-tooltip-width pos-tip-show-no-propertize
       do-smooth-scroll
       diminish
       lsp lsp-deferred lsp-mode
       projectile-acquire-root projectile-project-name
       projectile-run-project projectile-install-project projectile-configure-project
       paredit-mode paredit-comment-dwim
       smartparens-mode
       vertico--exhibit
       consult-yank-replace
       transient-get-value
       ace-link ace-link-help ace-link-info
       yas-minor-mode yas-text yas-activate-extra-mode yas-deactivate-extra-mode
       yas-expand-snippet yas-lookup-snippet yas-hippie-try-expand)))

(provide 'nvp-macs-decls)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-macs-decls.el ends here
