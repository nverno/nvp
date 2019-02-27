;;; nvp-clojure.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/clojure-tools
;; Package-Requires: 
;; Created:  1 December 2016

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

;; [![Build Status](https://travis-ci.org/nverno/clojure-tools.svg?branch=master)](https://travis-ci.org/nverno/clojure-tools)

;;; Description:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'subr-x)
(require 'cider)
(autoload 'nvp-log "nvp-log")

(nvp-package-define-root :snippets)

;; -------------------------------------------------------------------
;;; Util

(defvar-local nvp-clojure-root nil)
(defun nvp-clojure-root ()
  (or nvp-clojure-root
      (setq nvp-clojure-root
            (locate-dominating-file buffer-file-name "project.clj"))))

;;; FIXME
(defun nvp-clojure--namespace ()
  (replace-regexp-in-string
   "_" "-"
   (replace-regexp-in-string
    "/" "."
    (string-remove-suffix
     "test/"
     (string-remove-suffix
      "src/"
      (string-remove-suffix
       ".clj"
       (substring (buffer-file-name) (nvp-clojure-root))))))))

(defsubst nvp-clojure--namespace-under-test ()
  (replace-regexp-in-string "-test" "" (nvp-clojure--namespace)))

;; -------------------------------------------------------------------
;;; Commands

(nvp-newline nvp-clojure-newline-dwim nil
  :pairs (("\[" "\]") ("{" "}")))

;; (defun nvp-clojure-align-vectors (beg end)
;;   (interactive "r")
;;   (align-regexp beg end "^ \\[[^ ]+\\(\\s-+\\)" 1 1 t))

;; FIXME:
;; Find documentation for given SYMBOL online.
(defun nvp-clojure-help-online (symbol)
  (interactive (list (nvp-read "Clojure Docs: " (symbol-at-point))))
  (cl-destructuring-bind (x &optional y)
      (split-string symbol "/")
    (browse-url
     (concat "http://clojuredocs.org/clojure."
             (if y x "core")
             (if (string= "" y) "" "/")
             (url-hexify-string (or y x))))))

;; -------------------------------------------------------------------
;;; nREPL

;; https://github.com/howardabrams/dot-files/blob/master/emacs-clojure.org
;; Sends the s-expression located before the point or the active
;; region to the REPL and evaluates it. Then the Clojure buffer is
;; activated as if nothing happened.
(defun nvp-clojure-send-and-evaluate-sexp ()
  (interactive)
  (if (not (region-active-p))
      (cider-insert-last-sexp-in-repl)
    (cider-insert-in-repl
     (buffer-substring (region-beginning) (region-end)) nil))
  (cider-switch-to-repl-buffer)
  (cider-repl-closing-return)
  (cider-switch-to-last-clojure-buffer)
  (message ""))

;; https://github.com/kototama/.emacs.d/blob/master/lisp/config.el
(defun nvp-clojure-send-ns ()
  (interactive)
  (save-excursion
    (goto-char 0)
    (when (re-search-forward ":require" nil 'move)
      (let ((pos-first-require (1+ (point))))
        (re-search-forward ")")
        (let* ((pos-last-require (1- (point)))
               (requires
                (buffer-substring-no-properties pos-first-require
                                                pos-last-require))
               (quoted-requires
                (replace-regexp-in-string "\n +\\(\\[\\)" "\n'[" requires))
               (form (concat "(require '" quoted-requires ")")))
          (cider-switch-to-repl-buffer)
          ;; (switch-to-buffer-other-window (cider-find-or-create-repl-buffer))
          (goto-char (max-char))
          (insert form))))))

;; Evaluate with `cider-eval-defun-at-point' or
;; `cider-pprint-eval-defun-at-point' when prefixed.
(defun nvp-clojure-clj-eval-defun-at-point (&optional arg)
  (interactive "P")
  (if arg
      (cond
       ((eq 16 (car-safe arg))
        (cider-eval-defun-at-point '(16)))
       (t (cider-pprint-eval-defun-at-point)))
    (cider-eval-defun-at-point)))

(defadvice clojure-test-run-tests (before save-first activate)
  (save-buffer))

(defadvice nrepl-load-current-buffer (before save-first activate)
  (save-buffer))

(provide 'nvp-clojure)
;;; nvp-clojure.el ends here
