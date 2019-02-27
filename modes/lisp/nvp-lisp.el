;;; nvp-lisp.el --- lisp helpers  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Maintainer: Noah Peart <noah.v.peart@gmail.com>
;; Last modified: <2019-02-20 15:03:54>
;; URL: https://github.com/nverno/lisp-tools
;; Package-Requires: 
;; Created: 21 November 2016
;; Version: 0.0.1

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

;; [![Build Status](https://travis-ci.org/nverno/lisp-tools.svg?branch=master)](https://travis-ci.org/nverno/lisp-tools)

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'slime)

(declare-function slime-repl-eval-string "slime-repl")
(declare-function slime-switch-to-output-buffer "slime-repl")
(declare-function slime-repl-buffer "slime-repl")

(autoload 'nvp-elisp-abbrev-expand-var-p "nvp-elisp")
(autoload 'nvp-elisp-abbrev-expand-fn-p "nvp-elisp")

;;; TODO:
;; - auto-abbrevs
;; - sbcl: jump to source
;; - hap
;; - pophelp
;; - info

(nvp-package-define-root :snippets t)

;; -------------------------------------------------------------------
;;; Util

(defun nvp-lisp-slime-port (process)
  (let ((slime-port (or (process-id process)
                        (process-contact process))))
    (setq slime-port (cadr slime-port))
    slime-port))

(defun nvp-lisp-slime-buffer-list ()
  (let ((buf-list nil))
    (dolist (b (buffer-list))
      (when (string-match "*slime-repl sbcl*" (buffer-name b))
        (push (buffer-name b) buf-list)))
    buf-list))

;; -------------------------------------------------------------------
;;; Commands

;; evaluate buffer - if in .asd file, call asdf:load-system on it,
;; otherwise do regular slime eval-buffer
(defun nvp-lisp-eval-buffer (buffer-name)
  (interactive (list (buffer-name (current-buffer))))
  (if (not (string-match-p "asd" (file-name-extension buffer-name)))
      (call-interactively 'slime-eval-buffer)
    (let ((system (file-name-sans-extension buffer-name)))
      (slime-repl-eval-string (format "(asdf:load-system \"%s\")" system)))))

;; From http://bc.tech.coop/blog/070515.html
(defun nvp-lisp-lispdoc ()
  "Searches lispdoc.com for SYMBOL, which is by default the symbol 
currently under the curser."
  (interactive)
  (let* ((word-at-point (word-at-point))
	 (symbol-at-point (symbol-at-point))
	 (default (symbol-name symbol-at-point))
	 (inp (read-from-minibuffer
	       (if (or word-at-point symbol-at-point)
		   (concat "Symbol (default " default "): ")
		 "Symbol (no default): "))))
    (if (and (string= inp "")
             (not word-at-point)
             (not symbol-at-point))
	(message "you didn't enter a symbol!")
      (let ((search-type (read-from-minibuffer
			  "full-text (f) or basic (b) search (default b)? ")))
	(browse-url (concat "http://lispdoc.com?q="
			    (if (string= inp "")
				default
			      inp)
			    "&search="
			    (if (string-equal search-type "f")
				"full+text+search"
			      "basic+search")))))))

;; -------------------------------------------------------------------
;;; Hippie Expand

(eval-when-compile
  (defvar slime-repl-input-history)
  (defvar hippie-expand-try-functions-list)
  (defvar hippie-expand-only-buffers))
(declare-function slime-fuzzy-completions "slime-fuzzy")
(nvp-declare "" nvp-he-history-setup nvp-he-try-expand-local-abbrevs
  nvp-he-try-expand-flex-lisp nvp-he-history-remove-trailing-paren)
(autoload 'yas-hippie-try-expand "yasnippet")

(defvar nvp-lisp-he-expand-functions
  '(nvp-he-try-expand-dabbrev-closest-first
    nvp-he-try-expand-local-abbrevs
    nvp-he-try-expand-flex-lisp
    yas-hippie-try-expand
    try-expand-dabbrev-all-buffers
    try-expand-dabbrev-from-kill
    try-expand-slime-fuzzy
    try-complete-file-name-partially
    try-complete-file-name))

;; pass to `nvp-he-try-expand-history-trans'
(defsubst nvp-lisp-get-history-fn ()
  slime-repl-input-history)

(defun nvp-lisp-hippie-expand-setup (&optional repl)
  (setq-local hippie-expand-try-functions-list nvp-lisp-he-expand-functions)
  (setq-local hippie-expand-only-buffers '(lisp-mode))
  (when repl
    (nvp-he-history-setup :history 'slime-repl-input-history
                          :bol-fn #'line-beginning-position
                          :history-fn #'nvp-lisp-get-history-fn
                          :expand-fn #'nvp-he-history-remove-trailing-paren)))

;; -------------------------------------------------------------------
;;; REPL

(defun nvp-lisp-start-slime ()
  (interactive)
  (if (slime-connected-p)
      (if (< (length slime-net-processes) 2)
          (slime)
        (slime-list-connections))
    (slime)))

;; switch b/w repl and source
(nvp-repl-switch "lisp" (:repl-mode 'slime-repl-mode
                         :repl-live-p #'(lambda (&rest _i) t)
                         :repl-find-fn
                         #'(lambda ()
                             (slime-switch-to-output-buffer)
                             (slime-repl-buffer nil slime-buffer-connection))
                         :repl-switch-fn nil
                         :repl-process slime-buffer-connection))

(provide 'nvp-lisp)
;;; nvp-lisp.el ends here
