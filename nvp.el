;;; nvp.el --- base configs -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-02-21 13:37:51>
;; Package-Requires: 
;; Created:  2 November 2016
;; Version: 1.0.0

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; [![Build Status](https://travis-ci.org/nverno/nvp.svg?branch=master)](https://travis-ci.org/nverno/nvp)

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(require 'nvp-local)
(declare-function company-grab-symbol "company")

;;; TODO:
;; - only search in setup when not loaded
;;   `load-history-regexp', `load-history-filename-element'
;; - use mode-local ?, better way to set many mode-local variables

;;; Aliases
(defalias 'nvp-completing-read 'ido-completing-read)
(defalias 'nvp-grab-symbol 'company-grab-symbol)

;; root directory
(nvp-package-define-root :snippets t)

;; -------------------------------------------------------------------
;;; Variables

;; movement
(defvar-local nvp-move-header-regex nil "Regex to move b/w headers.")

;; Abbrevs
(defvar-local nvp-abbrev-local-file nil "File containing local abbrev tables.")
(defvar-local nvp-abbrev-local-table nil "Abbrev table to use for mode.")
(defvar-local nvp-abbrev-dynamic-table nil "On-the-fly abbrev table.")
(defvar nvp-abbrev-prefix-chars ":<>=/#.[:alnum:]"
  "Chars to include in abbrev prefixes")

;; Snippets
(defvar-local nvp-snippet-dir nil "Current mode's snippet directory.")

;; programs
(nvp-defvar nvp-program-search-paths
  (nvp-with-gnu/w32 `(,nvp/bin "~/.asdf/shims" "~/.local/bin" "/usr/local/bin")
    `(,nvp/bin ,nvp/binw)))

;; jumping variables -- might be set in dir-locals
(defvar nvp-default-org-file "gtd.org")
(nvp-defvar nvp-default-hooks-file (expand-file-name "nvp-mode-hooks.el" nvp/lisp))
(nvp-defvar nvp-build-init-dir (expand-file-name "build" nvp/home))
(defvar-local nvp-notes-local-file ())
(defvar-local nvp-books-local-directory ())

;; installs
(nvp-defvar nvp-install-makefile (expand-file-name "Makefile" nvp/install))
(defvar-local nvp-install-mode-targets ()
  "External installation targets for a major-mode.")

;; minibuffer read history from jumping to local configs
(defvar nvp-read-config-history ())

;; -------------------------------------------------------------------
;;; Functions
(nvp-declare "" nvp-ert-run-tests)

(defvar-local nvp-help-at-point-functions ())
(defvar-local nvp-check-buffer-function #'checkdoc)
(defvar-local nvp-disassemble-function #'disassemble)
(defvar-local nvp-repl-switch-function #'ignore
  "Function called to switch b/w source and REPL buffers.")
(defvar-local nvp-test-function #'nvp-ert-run-tests
  "Function called to run applicable tests at point.")
(defvar-local nvp-tag-function #'ignore
  "Function called to create tags by mode.")
(defvar-local nvp-mark-defun-function #'mark-defun)

;; -------------------------------------------------------------------
;;; Faces

(defface nvp-gaudy-variable-face
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

;; -------------------------------------------------------------------
;;; general helpers

;; (defsubst nvp-mode-config-path (mode)
;;   (expand-file-name (concat "nvp-" (nvp-stringify mode) "-config.el") nvp/config))

;; strip ctrl-m, multiple newlines
(defun nvp-process-buffer-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert (replace-regexp-in-string "[\r\n]+" "\n" string))))

;; add default to prompt in non-nil
(defsubst nvp-prompt--with-default (prompt &optional default)
  (if default (format "%s (default %s): "
                      (substring prompt 0 (string-match "[ :]+\\'" prompt)) default)
    prompt))

(provide 'nvp)
;;; nvp.el ends here
