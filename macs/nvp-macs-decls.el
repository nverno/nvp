;;; nvp-macs-decls.el --- silence compiler -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(require 'nvp-macs-common)

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
            (unless (or (nvp:ppss 'soc)
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
    nvp-mode-cache
    nvp-mode-header-regex
    nvp-mode-snippet-dir
    nvp-mode-install-targets
    nvp-mode-font-additions
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
    nvp-abbrev-dynamic-table nvp-abbrev-local-file nvp-abbrev-local-table
    nvp-abbrev-prefix-chars
    nvp-repl-alist
    nvp-repl-current
    nvp-repl-default
    nvp-repl-find-functions
    nvp-help-at-point-functions
    nvp-check-buffer-function
    nvp-disassemble-function
    nvp-tag-function
    nvp-compile-function
    nvp-mark-defun-function
    nvp-test-function
    nvp-format-buffer-function
    ;; fallbacks
    nvp-exit
    nvp-fallback-function
    nvp-fallback-minibuffer-function
    ;; hippie expansion variables
    nvp-he-flex-matcher nvp-he-flex-prefix-to-re nvp-he-flex-prefix-from-re
    nvp-he-flex-symbol-beg nvp-he-case-fold-search
    ;; builtin hippie
    hippie-expand-try-functions-list hippie-expand-only-buffers
    ;;=== emacs base ===
    package-user-dir
    recentf-list
    ielm-working-buffer ielm-dynamic-return
    org-babel-do-load-languages org-src-lang-modes
    local-abbrev-table
    bug-reference-bug-regexp
    bug-reference-url-format
    ;; align
    align-to-tab-stop
    ;; ido
    ido-default-buffer-method
    ido-default-file-method
    ;;=== external pkg ===
    zeal-at-point-docset
    dash-docs-docsets
    company-candidates company-selection
    ;; yasnippet
    yas-selected-text yas-wrap-around-region
    ;; ace-link
    ace-link-fallback-function
    ;; vertico
    vertico--input
    ;; url
    url-http-end-of-headers
    url-request-method url-request-extra-headers url-request-data))

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
       nvp-repeat-command
       nvp-autoload-keymap
       nvp-mark-defun
       nvp-move-previous-heading
       nvp-move-forward-heading
       nvp-mode-header-regex
       nvp-newline-dwim--comment
       ;; util
       nvp-regex-map-across-matches
       ;; install / package
       nvp-pkg-directory-dwim
       ;; project
       nvp-project-root
       nvp-project-parent
       ;; nvp-cycle.el
       nvp-cycle
       ;; reading
       nvp-read-elisp-symbol
       nvp-read-elisp-function
       nvp-read-elisp-variable
       nvp-completing-read
       nvp-find-file-in-dir
       ;; modes
       nvp-scratch-minor-mode
       ;; logging
       nvp-results-title
       nvp-log-mode
       nvp-log
       nvp-log-default
       nvp-display-fallback-function
       nvp-display-fallback-dired
       ;; windows
       nvp-window-configuration-restore
       nvp-window-configuration-save
       ;; hippie
       nvp-try-expand-history
       nvp-he-history-setup
       nvp-he-history-remove-trailing-paren
       nvp-try-expand-flex
       nvp-he-flex-lisp
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
       nvp-repl-cd-here
       nvp-repl-menu
       nvp-shell
       nvp-shell-launch-terminal
       ;; indication
       nvp-indicate-pulse-region-or-line
       nvp-indicate-modeline
       nvp-indicate-cursor-pre
       nvp-indicate-cursor-post
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
       nvp-abbrev-grab
       nvp-grab-symbol
       nvp-abbrev-expand-p
       nvp-abbrev-expand-not-after-punct-p
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
       nvp-setup-program
       nvp-setup-local
       nvp-lookup-password
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
       org-comment-dwim)

     (nvp:decl                          ; external packages
       pos-tip-show
       projectile-project-name
       do-smooth-scroll
       ;; paredit
       paredit-comment-dwim
       ;; vertico
       vertico--exhibit
       ;; consult
       consult-yank-replace
       ;; transient
       transient-get-value
       ;; ace-link
       ace-link
       ace-link-help
       ace-link-info
       ;; yasnippet
       yas-expand-snippet
       yas-lookup-snippet
       yas-hippie-try-expand)))

(provide 'nvp-macs-decls)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-macs-decls.el ends here
