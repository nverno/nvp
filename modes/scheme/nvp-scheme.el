;;; nvp-scheme.el --- scheme helpers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/scheme-tools
;; Last modified: <2019-02-20 13:27:45>
;; Package-Requires: 
;; Created: 14 May 2017

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

;; [![Build Status](https://travis-ci.org/nverno/scheme-tools.svg?branch=master)](https://travis-ci.org/nverno/scheme-tools)

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (nvp-local-vars)
  (require 'cl-lib)
  (defvar company-selection)
  (defvar company-candidates)
  (defvar geiser-active-implementations)
  (defvar geiser-impl--implementation)
  (defvar geiser-impl--implementations))
(require 'geiser)
(declare-function geiser-eval-last-sexp "geiser-mode")
(declare-function nvp-ext-run-script "nvp-ext")
(declare-function nvp-log "nvp-log")

(nvp-package-define-root :snippets t)

;;; TODO:
;; - snippets => jump to snippet should depend on geiser-impl--implementation?

;; -------------------------------------------------------------------
;;; Utils

;;; FIXME: use proper paths
;; determine available scheme implementations
(defun nvp-scheme-active-implementations ()
  (let (res)
    (mapc
     (lambda (i)
       (let* ((local
               (expand-file-name (symbol-name i) "/usr/local/bin"))
              (prog (or (and (file-exists-p local) local)
                        (executable-find (symbol-name i)))))
         (when prog
           (push i res))))
     geiser-active-implementations)
    res))

;; -------------------------------------------------------------------
;;; Abbrevs

;; Don't expand in strings, comments, or function arguments. And don't
;; expand after '-' or '/' for christ.
(defun nvp-scheme-abbrev-expand-p ()
  (when (not (memq last-input-event '(?/ ?- ?\?)))
    (or (memq this-command '(expand-abbrev))
        (let ((ppss (syntax-ppss)))
          (unless (or (elt ppss 3)      ;not in strings/comments
                      (elt ppss 4))
            (save-match-data
              (and (looking-back "([[:alnum:]]*"
                                 (line-beginning-position))
                   (save-excursion
                     (goto-char (match-beginning 0))
                     (and
                      ;; don't expand in function/lambda arguments
                      (not (looking-back "(\\(?:lambda\\|define\\) +"
                                         (line-beginning-position))
                           ;; (looking-back
                           ;;  "(define +\\(?:\\sw\\|\\s_\\)+ +"
                           ;;  (line-beginning-position))
                           )
                      (condition-case nil
                          (progn
                            (up-list)
                            (backward-sexp)
                            (not
                             (or
                              (looking-back
                               "(let\\*? *"
                               (line-beginning-position)))))
                        (error t)))))))))))

;; set local abbrev table according to implementation
(eval-when-compile
  (defvar nvp-abbrev-local-table))
(defun nvp-scheme-abbrev-table ()
  (setq-local nvp-abbrev-local-table (format "%s-mode" geiser-impl--implementation))
  (setq local-abbrev-table
        (symbol-value (intern (format "%s-abbrev-table"
                                      nvp-abbrev-local-table)))))

(defadvice geiser-set-scheme (after set-abbrev-table activate)
  (nvp-scheme-abbrev-table))

;; -------------------------------------------------------------------
;;; Hippie Expand

(eval-when-compile
  (defvar hippie-expand-try-functions-list)
  (defvar hippie-expand-only-buffers))
(nvp-declare "" nvp-he-history-setup nvp-he-try-expand-flex-lisp
  nvp-he-try-expand-local-abbrevs)
(autoload 'yas-hippie-try-expand "yasnippet")

(defvar nvp-scheme-he-expand-functions
  '(nvp-he-try-expand-dabbrev-closest-first
    nvp-he-try-expand-local-abbrevs
    nvp-he-try-expand-flex-lisp
    yas-hippie-try-expand
    try-expand-dabbrev-from-kill
    try-expand-dabbrev-all-buffers
    try-complete-file-name-partially
    try-complete-file-name))

(defun nvp-scheme-hippie-expand-setup (&optional repl)
  (setq-local hippie-expand-try-functions-list nvp-scheme-he-expand-functions)
  (setq-local hippie-expand-only-buffers '(scheme-mode))
  (when repl
    (nvp-he-history-setup :history 'comint-input-ring
                          :bol-fn 'comint-line-beginning-position
                          :expand-fn 'nvp-he-history-remove-trailing-paren)))

;; -------------------------------------------------------------------
;;; REPL

(eval-when-compile
  (defvar local-abbrev-table))
(nvp-declare "abbrev" abbrev-table-put abbrev-table-get)

;; enable abbrevs according to scheme implementiation
;; since `geiser-impl--implementation' isn't available when this
;; hook is run, the hack here should set the parent abbrev table
;; and remove the enable function the first time expand-abbrev is called
;;
;; This removes previous parent tables unless SAVE-PARENTS is non-nil,
;; in case scheme was changed b/w REPL invocations
(defun nvp-scheme-repl-init-abbrevs (&optional save-parents)
  (let ((impl geiser-impl--implementation))
    (when impl
      (let ((parents (and save-parents
                          (abbrev-table-get local-abbrev-table :parents)))
            (table (concat (symbol-name impl) "-mode-abbrev-table")))
        (abbrev-table-put
         local-abbrev-table
         :parents (cons (symbol-value (intern table)) parents)))))
  ;; now remove enable hook
  (abbrev-table-put local-abbrev-table :enable-function nil))

;; -------------------------------------------------------------------
;;; Commands

(defun nvp-scheme-eval-last-sexp ()
  "Move to end of current sexp and evaluate."
  (interactive)
  (unless (looking-back ")\\s-*" (line-beginning-position))
    (forward-sexp 1)
    (goto-char (1+ (point))))
  (geiser-eval-last-sexp current-prefix-arg))

;; geiser repl broken end-of-line after history
(defun nvp-scheme-inf-end-of-line ()
  (interactive)
  (let ((inhibit-field-text-motion t))
    (end-of-line)))

(provide 'nvp-scheme)
;;; nvp-scheme.el ends here