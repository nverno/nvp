;;; nvp-macs-decls.el --- silence compiler -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(require 'nvp-macs-common)

(defmacro nvp-local-defvars (&rest defvars)
  (macroexp-progn
   (cl-loop for dv in defvars
      collect `(defvar ,dv))))

(defmacro nvp-local-vars ()
  `(nvp-local-defvars
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
    nvp-default-log-function
    nvp-default-org-file
    nvp-window-configuration-stack
    nvp-program-search-paths
    nvp-abbrev-dynamic-table
    nvp-abbrev-local-file
    nvp-abbrev-local-table
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
    ;; emacs base
    explicit-shell-file-name
    ielm-working-buffer
    ielm-dynamic-return
    imenu-generic-expression
    tramp-remote-path
    tramp-default-user
    org-babel-do-load-languages
    org-src-lang-modes
    ;; external pkgs
    company-backends
    company-dabbrev-code-modes
    yas-snippet-dirs yas-text yas-moving-away-p yas-modified-p
    zeal-at-point-docset))

(defmacro nvp-decls ()
  '(progn
     (nvp-local-vars)
     (nvp-decl :pre "nvp"
       ;; general generics
       mark-defun
       ;; movement
       move-previous-heading move-forward-heading mode-header-regex
       ;; bindings
       bind-transient-key
       ;; reading
       read-elisp-symbol read-elisp-function read-elisp-variable
       completing-read find-file-in-dir
       ;; modes
       view-list-mode scratch-minor-mode
       ;; logging
       log-mode log log-default
       ;; windows
       window-configuration-restore window-configuration-save
       ;; hippie
       he-try-expand-history he-history-setup he-history-remove-trailing-paren
       he-try-expand-flex-lisp try-expand-dabbrev-closest-first
       he-try-expand-local-abbrevs
       ;; repls / shell
       comint-setup-history
       repl-add
       shell shell-launch-terminal
       ;; indication
       indicate-pulse-region-or-line indicate-modeline
       indicate-cursor-pre indicate-cursor-post
       ;; procs
       proc-default-filter proc-default-sentinel
       ;; imenu
       imenu-setup idomenu
       ;; toggle
       toggle-local-variable
       ;; abbrev
       abbrev-grab grab-symbol
       ;; test / compile
       ert-run-tests
       compile compile-default compile-cmake
       ;; strings
       s-repeat s-center
       ;; environment
       env-add env-path-add
       ;; setup
       setup-program
       lookup-password)

     (nvp-decl                          ; builtins
       comint-read-input-ring comint-write-input-ring
       ielm ielm-return
       ert-run-tests-interactively
       hs-already-hidden-p hs-show-all hs-show-block hs-hide-all hs-hide-block
       tramp-dissect-file-name tramp-make-tramp-file-name
       w32-shell-execute)

     (nvp-decl                          ; external packages
       pos-tip-show
       do-smooth-scroll
       minibuffer-keyboard-quit
       company-grab-symbol company-mode
       smartparens-mode sp-local-pair
       paredit-mode
       yas-minor-mode yas-expand-snippet yas-lookup-snippet yas-load-directory)))

(provide 'nvp-macs-decls)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-macs-decls.el ends here
