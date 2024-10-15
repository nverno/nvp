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
    nvp/history
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
    nvp/lisp/src
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

    ace-link-fallback-function
    align-to-tab-stop
    bug-reference-bug-regexp bug-reference-url-format
    company-candidates company-selection
    compilation-error-face compilation-warning-face compilation-info-face
    compilation-error-regexp-alist compilation-error-regexp-alist-alist
    crm-separator
    dash-docs-docsets
    devdocs-current-docs
    explicit-shell-file-name
    he-search-string he-tried-table he-expand-list
    hippie-expand-try-functions-list hippie-expand-only-buffers
    ido-default-buffer-method ido-default-file-method
    ielm-working-buffer ielm-dynamic-return
    imenu-generic-expression imenu--index-alist imenu-auto-rescan
    local-abbrev-table
    lsp-auto-configure
    nvp-abbrev-default-function
    nvp-abbrev-prefix-chars
    nvp-abbrevd-obarray nvp-local-abbrev-file nvp-local-abbrev-table
    nvp-check-buffer-default-function
    nvp-compile-default-function
    nvp-compile-default-function
    nvp-configure-default-function
    nvp-debug-breakpoint-string
    nvp-debug-default-function
    nvp-default-log-function
    nvp-default-org-file
    nvp-disassemble-default-function
    nvp-display-buffer-other-frame-action nvp-display-buffer-same-frame-action
    nvp-display-buffer-other-window-action nvp-display-buffer-same-window-action
    nvp-display-fallback-function
    nvp-exit
    nvp-fallback-function
    nvp-fallback-minibuffer-function
    nvp-format-buffer-default-function
    nvp-he-flex-matcher nvp-he-flex-prefix-to-re nvp-he-flex-prefix-from-re
    nvp-he-flex-symbol-beg nvp-he-case-fold-search
    nvp-help-at-point-functions
    nvp-install-default-function
    nvp-local-bookmark-file
    nvp-local-books-directories
    nvp-local-compile-function
    nvp-local-notes-file
    nvp-local-src-directories
    nvp-local-uris
    nvp-mark-defun-function
    nvp-mode-cache
    nvp-mode-default-functions
    nvp-mode-font-additions
    nvp-mode-function-hooks
    nvp-mode-header-regex
    nvp-mode-hooks
    nvp-mode-name
    nvp-mode-snippet-dir
    nvp-program-search-paths
    nvp-repl-alist
    nvp-repl-current
    nvp-repl-default
    nvp-repl-find-functions
    nvp-run-default-function
    nvp-tabulated-list-select-action
    nvp-tag-default-function
    nvp-tags-ctags-program
    nvp-test-default-function
    nvp-toggle-default-function
    nvp-window-configuration-stack
    org-babel-do-load-languages org-src-lang-modes
    package-user-dir
    projectile-tags-command
    projectile-tags-exclude-patterns
    projectile-tags-file-name
    recentf-list
    repeat-in-progress
    treesit-font-lock-feature-list
    treesit-font-lock-settings
    url-http-end-of-headers
    url-request-method url-request-extra-headers url-request-data
    vertico--input
    yas-text yas-selected-text yas-wrap-around-region
    zeal-at-point-docset))

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
       nvp-abbrev-expand-after-symbols-hook
       nvp-abbrev-expand-not-after-punct-p
       nvp-abbrev-expand-p
       nvp-abbrev-expand-strictly
       nvp-abbrev-from-splitters
       nvp-abbrev-from-words
       nvp-abbrev-grab
       nvp-abbrev-make-post-insert-hook
       nvp-abbrev-write-abbrev-table
       nvp-abbrevd
       nvp-async-shell-command-to-string
       nvp-autoload-keymap
       nvp-buffer-local-set-key
       nvp-buffer-local-set-minor-mode-key
       nvp-cache-create
       nvp-cache-get
       nvp-cmake-compile
       nvp-comint-setup-history
       nvp-company-local
       nvp-compile
       nvp-compile-default
       nvp-completing-read
       nvp-cycle
       nvp-display-buffer-action
       nvp-display-buffer-other-window
       nvp-display-fallback-function
       nvp-display-window-get-arguments
       nvp-env-add
       nvp-env-path-add
       nvp-ert-run-tests
       nvp-eval-expression nvp-eval--display-expression
       nvp-fallback-dired
       nvp-find-file-in-dir
       nvp-grab-symbol
       nvp-hap-company nvp-hap-semantic nvp-hap-treesit nvp-hap-local nvp-hap-word
       nvp-he-chained-symbol-beg
       nvp-he-elisp-setup
       nvp-he-flex-camel/snake
       nvp-he-flex-lisp
       nvp-he-flex-lisp-setup
       nvp-he-history-remove-trailing-paren
       nvp-he-history-setup
       nvp-help-at-point nvp-hap-elisp nvp-hap-info nvp-hap-man nvp-hap-lsp
       nvp-imenu
       nvp-imenu-setup
       nvp-imenu-wrapper
       nvp-indicate-abort nvp-indicate-cursor-pre nvp-indicate-cursor-post
       nvp-indicate-modeline
       nvp-indicate-pulse-region-or-line
       nvp-inf-read-process
       nvp-install-mode nvp-install-modes
       nvp-list-wrap
       nvp-log
       nvp-log-default
       nvp-log-mode
       nvp-lookup-password
       nvp-mark-defun nvp-mark-expand-to-previous-comments
       nvp-mode-header-regex
       nvp-move-previous-heading nvp-move-forward-heading
       nvp-narrow-dwim
       nvp-newline-dwim--comment
       nvp-org-links
       nvp-paredit-close-round
       nvp-pkg-directory-dwim
       nvp-proc-default-filter
       nvp-proc-default-sentinel
       nvp-project-root nvp-project-parent nvp-project-locate-root
       nvp-read-elisp-function
       nvp-read-elisp-symbol
       nvp-read-elisp-variable
       nvp-read-mode
       nvp-regex-map-across-matches
       nvp-repeat-set-cursor nvp-repeat-abort
       nvp-repl-add
       nvp-repl-cd
       nvp-repl-config
       nvp-repl-eval-result-value
       nvp-repl-eval-sexp
       nvp-repl-eval-show-result
       nvp-repl-eval-show-result
       nvp-repl-eval-string
       nvp-repl-help
       nvp-repl-jump
       nvp-repl-menu
       nvp-repl-pwd
       nvp-repl-send-buffer
       nvp-repl-send-defun
       nvp-repl-send-line
       nvp-repl-send-region
       nvp-repl-send-string
       nvp-repl-setup-input-filter
       nvp-repl-show-result
       nvp-results-title
       nvp-scratch-minor-mode
       nvp-setup-program nvp-setup-local
       nvp-shell
       nvp-shell-launch-terminal
       nvp-tag-find nvp-tag-find-etag
       nvp-tag-list-decls
       nvp-tag-list-tags nvp-tag-list-decls nvp-tag-repo
       nvp-theme-switch
       nvp-toggle-local-variable
       nvp-try-expand-dabbrev-closest-first
       nvp-try-expand-flex
       nvp-try-expand-history
       nvp-try-expand-local-abbrevs
       nvp-window-configuration-restore
       nvp-window-configuration-save
       nvp-xref-go nvp-etags-find-definitions)

     (nvp:decl                          ; builtins
       ert-run-tests-interactively
       find-function-other-window
       he-substitute-string he-reset-string he-string-member he-init-string
       hs-already-hidden-p hs-show-all hs-show-block hs-hide-all hs-hide-block
       ielm ielm-return
       imenu--make-index-alist imenu-default-create-index-function
       minibuffer-keyboard-quit
       org-comment-dwim
       treesit-font-lock-recompute-features
       treesit-font-lock-rules
       w32-shell-execute
       xref-pop-marker-stack xref-push-marker-stack)

     (nvp:decl                          ; external packages
       ace-link ace-link-help ace-link-info
       consult-yank-replace consult-recent-file
       diminish
       do-smooth-scroll
       edit-indirect
       lsp lsp-deferred lsp-mode
       paredit-mode paredit-comment-dwim
       pos-tip-show pos-tip-tooltip-width pos-tip-show-no-propertize
       projectile-acquire-root projectile-project-name
       projectile-run-project projectile-install-project projectile-configure-project
       smartparens-mode
       transient-get-value
       vertico--exhibit
       yas-expand-snippet yas-lookup-snippet yas-hippie-try-expand
       yas-minor-mode yas-text yas-activate-extra-mode yas-deactivate-extra-mode)))

(provide 'nvp-macs-decls)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-macs-decls.el ends here
