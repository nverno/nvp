;;; nvp.el --- base configs -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-02-14 05:34:41>
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
;; - separate setup from variables
;; - only search in setup when not loaded
;;   `load-history-regexp', `load-history-filename-element'
;; - use mode-local ?, better way to set many mode-local variables

;;; Aliases
(defalias 'nvp-completing-read 'ido-completing-read)
(defalias 'nvp-grab-symbol 'company-grab-symbol)
(defalias 'nvp-move-previous-defun 'beginning-of-defun)

;; root directory
(nvp-package-define-root)

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
(defvar-local nvp-snippet-dir nil "Directory to load for mode's snippets.")

;; programs
(defvar nvp-program-search-paths
  (nvp-with-gnu/w32 `(,nvp/bin "~/.asdf/shims" "~/.local/bin" "/usr/local/bin")
    `(,nvp/bin ,nvp/binw)))

;; local variables for jumping -- might be set in dir-locals
(defvar-local nvp-notes-local-file ())
(defvar-local nvp-books-local-directory ())

;; installs
(defvar-local nvp-install-mode-targets ()
  "External installation targets for a major-mode.")

;; -------------------------------------------------------------------
;;; Functions
;; (defvar-local nvp-help-at-point-function #'nvp-help-at-point)
;; (defvar-local nvp-repl-switch-function #'nvp-repl-switch)
(defvar-local nvp-check-buffer-function #'checkdoc)
(defvar-local nvp-disassemble-function #'disassemble)

;; -------------------------------------------------------------------
;;; general helpers

;; (defsubst nvp-mode-config-path (mode)
;;   (expand-file-name (concat "nvp-" (nvp-stringify mode) "-config.el") nvp/config))

;; strip ctrl-m, multiple newlines
(defun nvp-process-buffer-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert (replace-regexp-in-string "[\r\n]+" "\n" string))))

(provide 'nvp)
;;; nvp.el ends here
