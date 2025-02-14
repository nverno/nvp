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
    nvp/backup
    nvp/bin
    nvp/binw
    nvp/bookmark
    nvp/books
    nvp/build
    nvp/cache
    nvp/class
    nvp/config
    nvp/custom
    nvp/cygwin
    nvp/data
    nvp/devel
    nvp/emacs
    nvp/git
    nvp/history
    nvp/info
    nvp/install
    nvp/lisp
    nvp/lisp/src
    nvp/modes
    nvp/msys
    nvp/org
    nvp/private
    nvp/project
    nvp/scratch
    nvp/site
    nvp/snippet
    nvp/template
    nvp/test
    nvp/vms
    nvp/work

    ace-link-fallback-function
    align-to-tab-stop
    bug-reference-bug-regexp
    bug-reference-url-format
    cl-print-readably
    company-backends
    company-candidates
    company-selection
    compilation-error-face
    compilation-error-regexp-alist
    compilation-error-regexp-alist-alist
    compilation-info-face
    compilation-warning-face
    crm-separator
    dash-docs-docsets
    devdocs-current-docs
    explicit-shell-file-name
    he-expand-list
    he-search-string
    he-tried-table
    hippie-expand-only-buffers
    hippie-expand-try-functions-list
    ido-default-buffer-method
    ido-default-file-method
    ielm-dynamic-return ielm-working-buffer
    imenu--index-alist
    imenu-auto-rescan
    imenu-generic-expression
    local-abbrev-table
    lsp-auto-configure
    lsp-language-id-configuration
    nvp-abbrev-default-function
    nvp-abbrev-prefix-chars
    nvp-abbrevd-obarray
    nvp-check-buffer-default-function
    nvp-compile-default-function
    nvp-compile-default-function
    nvp-configure-default-function
    nvp-debug-breakpoint-string
    nvp-debug-default-function
    nvp-default-log-function
    nvp-default-org-file
    nvp-disassemble-default-function
    nvp-display-buffer-other-frame-action
    nvp-display-buffer-other-window-action
    nvp-display-buffer-same-frame-action
    nvp-display-buffer-same-window-action
    nvp-display-fallback-function
    nvp-display-prefix-p
    nvp-exit
    nvp-fallback-function
    nvp-fallback-minibuffer-function
    nvp-format-buffer-default-function
    nvp-he-case-fold-search
    nvp-he-flex-matcher
    nvp-he-flex-prefix-from-re
    nvp-he-flex-prefix-to-re
    nvp-he-flex-symbol-beg
    nvp-help-at-point-functions
    nvp-install-default-function
    nvp-local-abbrev-file
    nvp-local-abbrev-table
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
    org-babel-do-load-languages
    org-src-lang-modes
    package-user-dir
    projectile-tags-command
    projectile-tags-exclude-patterns
    projectile-tags-file-name
    recentf-list
    repeat-in-progress
    treesit-font-lock-feature-list
    treesit-font-lock-settings
    url-http-end-of-headers
    url-request-data
    url-request-extra-headers
    url-request-method
    vertico--input
    yas-selected-text
    yas-text
    yas-wrap-around-region
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
       nvp-other-window-scroll-default
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
       nvp-display-window-get-arguments
       nvp-env-add
       nvp-env-path-add
       nvp-ert-run-tests
       nvp-etags-find-definitions
       nvp-eval--display-expression
       nvp-pp-form
       nvp-eval-expression
       nvp-fallback-dired
       nvp-find-file-in-dir
       nvp-grab-symbol
       nvp-hap-company
       nvp-hap-elisp
       nvp-hap-info
       nvp-hap-local
       nvp-hap-lsp
       nvp-hap-man
       nvp-hap-semantic
       nvp-hap-treesit
       nvp-hap-word
       nvp-he-chained-symbol-beg
       nvp-he-elisp-setup
       nvp-he-flex-camel/snake
       nvp-he-flex-lisp
       nvp-he-flex-lisp-setup
       nvp-he-history-remove-trailing-paren
       nvp-he-history-setup
       nvp-help-at-point
       nvp-imenu
       nvp-imenu-setup
       nvp-imenu-wrapper
       nvp-indicate-abort
       nvp-indicate-cursor-post
       nvp-indicate-cursor-pre
       nvp-indicate-modeline
       nvp-indicate-pulse-region-or-line
       nvp-inf-read-process
       nvp-install-mode
       nvp-install-modes
       nvp-list-wrap
       nvp-log
       nvp-log-default
       nvp-log-mode
       nvp-lookup-password
       nvp-mark-defun
       nvp-mark-expand-to-previous-comments
       nvp-mode-header-regex
       nvp-move-forward-heading
       nvp-move-previous-heading
       nvp-narrow-dwim
       nvp-newline-dwim--comment
       nvp-org-links
       nvp-paredit-close-round
       nvp-pkg-directory-dwim
       nvp-proc-default-filter
       nvp-proc-default-sentinel
       nvp-project-locate-root
       nvp-project-parent
       nvp-project-root
       nvp-read-elisp-function
       nvp-read-elisp-symbol
       nvp-read-elisp-variable
       nvp-read-mode
       nvp-regex-map-across-matches
       nvp-repeat-abort
       nvp-repeat-set-cursor
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
       nvp-shell
       nvp-shell-launch-terminal
       nvp-sort-alist
       nvp-tag-find
       nvp-tag-find-etag
       nvp-tag-list-decls
       nvp-tag-list-tags
       nvp-tag-repo
       nvp-theme-switch
       nvp-toggle-local-variable
       nvp-try-expand-dabbrev-closest-first
       nvp-try-expand-flex
       nvp-try-expand-history
       nvp-try-expand-local-abbrevs
       nvp-window-configuration-restore
       nvp-window-configuration-save
       nvp-xref-go)

     (nvp:decl                          ; builtins
       ert-run-tests-interactively
       find-function-other-window
       he-init-string
       he-reset-string
       he-string-member
       he-substitute-string
       hs-already-hidden-p
       hs-hide-all
       hs-hide-block
       hs-show-all
       hs-show-block
       ielm
       ielm-return
       imenu--make-index-alist
       imenu-default-create-index-function
       minibuffer-keyboard-quit
       org-comment-dwim
       treesit-font-lock-recompute-features
       treesit-font-lock-rules
       w32-shell-execute
       xref-pop-marker-stack
       xref-push-marker-stack)

     (nvp:decl                          ; external packages
       ace-link
       ace-link-help
       ace-link-info
       consult-recent-file
       consult-recent-file
       consult-yank-replace
       diminish
       do-smooth-scroll
       edit-indirect
       lsp
       lsp-deferred
       lsp-mode
       paredit-comment-dwim
       paredit-mode
       pos-tip-show
       pos-tip-show-no-propertize
       pos-tip-tooltip-width
       projectile-acquire-root
       projectile-configure-project
       projectile-install-project
       projectile-project-name
       projectile-run-project
       smartparens-mode
       transient-get-value
       vertico--exhibit
       yas-activate-extra-mode
       yas-deactivate-extra-mode
       yas-expand-snippet
       yas-hippie-try-expand
       yas-lookup-snippet
       yas-minor-mode
       yas-text)))

(provide 'nvp-macs-decls)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-macs-decls.el ends here
