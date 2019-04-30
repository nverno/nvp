;;; nvp-macs-decls.el --- silence compiler -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(require 'cl-lib)
(eval-when-compile (require 'nvp-macs-common))

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
    ;; external pkgs
    company-backends
    yas-snippet-dirs))

(defmacro nvp-decls ()
  '(progn
     (nvp-local-vars)
     (nvp-decl :pre "nvp"
       read-elisp-symbol read-elisp-function read-elisp-variable
       completing-read find-file-in-dir
       view-list-mode scratch-minor-mode
       log
       window-configuration-restore window-configuration-save
       he-history-setup comint-setup-history
       indicate-pulse-region-or-line indicate-modeline
       indicate-cursor-pre indicate-cursor-post
       imenu-setup idomenu
       toggle-local-variable
       abbrev-grab grab-symbol
       mark-defun
       ert-run-tests
       compile compile-default compile-cmake
       repl-add
       shell shell-launch-terminal
       s-repeat s-center
       env-add env-path-add)
     (nvp-decl                          ; builtins
       comint-read-input-ring comint-write-input-ring
       ielm ielm-return
       ert-run-tests-interactively
       hs-already-hidden-p hs-show-all hs-show-block hs-hide-all hs-hide-block
       w32-shell-execute)
     (nvp-decl                          ; external packages
       pos-tip-show
       do-smooth-scroll
       minibuffer-keyboard-quit
       company-grab-symbol company-mode
       sp-local-pair
       paredit-mode
       yas-minor-mode yas-expand-snippet yas-lookup-snippet)))

(provide 'nvp-macs-decls)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-macs-decls.el ends here
