;;; nvp.el --- base configs -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-03-24 04:07:49>
;; Created:  2 November 2016

;;; Commentary:

;; [![Build Status](https://travis-ci.org/nverno/nvp.svg?branch=master)](https://travis-ci.org/nverno/nvp)

;; - Global variables / variables set by modes
;; - Some general aliases that haven't made it to init
;; - Local site variables - compiled in init
;; - some utility functions
;; - font faces

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(require 'nvp-local)
(declare-function company-grab-symbol "company")

;;; Aliases
(defalias 'nvp-completing-read 'ido-completing-read)
(defalias 'nvp-grab-symbol 'company-grab-symbol)

;; root directory
(nvp-package-define-root :snippets t)

;; -------------------------------------------------------------------
;;; Variables

;; movement
(defvar-local nvp-local-header-regex nil "Regex to move b/w headers.")

;; Abbrevs
(defvar-local nvp-abbrev-local-file nil "File containing local abbrev tables.")
(put 'nvp-local-header-regex 'permanent-local t)
(defvar-local nvp-abbrev-local-table nil "Abbrev table to use for mode.")
(defvar-local nvp-abbrev-dynamic-table nil "On-the-fly abbrev table.")
(defvar-local nvp-abbrev-prefix-chars ":<>=/#._[:alnum:]"
  "Chars to include in abbrev prefixes")
(put 'nvp-abbrev-prefix-chars 'permanent-local t)

;; Snippets
(defvar-local nvp-snippet-dir nil "Current mode's snippet directory.")

;; programs
(nvp-defvar nvp-program-search-paths
  (nvp-with-gnu/w32 `(,nvp/bin "~/.asdf/shims" "~/.local/bin" "/usr/local/bin")
    `(,nvp/bin ,nvp/binw)))

;; jumping variables -- might be set in dir-locals
(defvar nvp-default-org-file "gtd.org")
(nvp-defvar nvp-default-hooks-file (expand-file-name "nvp-mode-hooks.el" nvp/lisp))
(nvp-defvar nvp-build-init-dir (expand-file-name "build" nvp/emacs))
(defvar-local nvp-local-notes-file () "Local notes/todo to jump dwim.")
(put 'nvp-local-notes-file 'permanent-local t)
(defvar-local nvp-local-books-directories () "Local book directory/s.")
(put 'nvp-local-notes-file 'permanent-local t)
(defvar-local nvp-local-uris () "Local URIs for webjumping.")
(put 'nvp-local-uris 'permanent-local t)
(defvar-local nvp-local-src-directories () "Local source dirs to jump.")

;; windows
(defvar nvp-window-configuration-stack ())

;; installs
(nvp-defvar nvp-install-makefile (expand-file-name "Makefile" nvp/install))
(defvar-local nvp-install-mode-targets ()
  "External installation targets for a major-mode.")

;; font-lock
(defvar-local nvp-local-font-lock () "Local font-lock additions.")

;; minibuffer read history from jumping to local configs
(defvar nvp-read-config-history ())

;; -------------------------------------------------------------------
;;; Functions
(nvp-declare "" nvp-ert-run-tests nvp-compile-default)

(defvar-local nvp-help-at-point-functions ())
(defvar-local nvp-check-buffer-function #'checkdoc)
(defvar-local nvp-disassemble-function #'disassemble)
(defvar-local nvp-compile-function #'nvp-compile-default
  "Function to compile file.")
;; (defvar-local nvp-repl-jump-function #'ignore
;;   "Function called to switch b/w source and REPL buffers.")
(defvar-local nvp-test-function #'nvp-ert-run-tests
  "Function called to run applicable tests at point.")
(defvar-local nvp-tag-function #'ignore
  "Function called to create tags by mode.")
(defvar-local nvp-mark-defun-function #'mark-defun)

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
;;; general helpers

;; strip ctrl-m, multiple newlines
(defun nvp-process-buffer-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert (replace-regexp-in-string "[\r\n]+" "\n" string))))

;; add default to prompt in non-nil
(defsubst nvp-prompt-default (prompt &optional default)
  (if default (format "%s (default %s): "
                      (substring prompt 0 (string-match "[ :]+\\'" prompt)) default)
    prompt))

;; Read with default of symbol-at-point. When COLLECTION is provided use
;; completing read, otherwise read from the minibuffer
(defun nvp-read-default (prompt &optional collection pred match initial
                                           hist default inherit)
  (or default (setq default (symbol-at-point)))
  (or prompt (setq prompt (nvp-prompt-default)))
  (if collection
      (nvp-completing-read (nvp-prompt-default prompt default) collection pred
                           match initial hist default inherit)
    (read-from-minibuffer prompt nil nil nil nil default)))

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

(provide 'nvp)
;;; nvp.el ends here
