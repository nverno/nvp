;;; nvp-ruby.el --- rubls -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/ruby-tools
;; Package-Requires: 
;; Copyright (C) 2016, Noah Peart, all rights reserved.
;; Created:  1 November 2016

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

;; [![Build Status](https://travis-ci.org/nverno/ruby-tools.svg?branch=master)](https://travis-ci.org/nverno/ruby-tools)

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro)
  (nvp-local-vars)
  (defvar projectile-tags-file-name))
(require 'ruby-mode)
(require 'inf-ruby nil t)
(require 'robe nil t)
(nvp-declare ""
    robe-mode inf-ruby robe-start ruby-switch-to-inf
    ruby-send-block-and-go ruby-switch-to-last-ruby-buffer
    ruby-send-definition-and-go ruby-send-last-sexp ruby-send-region-and-go)
(declare-function ruby-compilation-this-buffer "ruby-compilation")
(declare-function projectile-rails-root "projectile-rails")

(nvp-package-define-root :snippets t)

;; -------------------------------------------------------------------
;;; Utils

(defsubst nvp-ruby-version ()
  (let ((str (shell-command-to-string "ruby --version")))
    (and (string-match "\\([0-9.]+\\)" str)
         (match-string-no-properties 1 str))))

;; convert file[_-.]name to FileName
(defsubst nvp-ruby-camelize (str &optional seps)
  (mapconcat 'identity
             (mapcar 'capitalize (split-string str (or seps "[._-]"))) ""))

;; gather required libs in file
(defun nvp-ruby-requires ()
  (save-excursion
    (goto-char (point-min))
    (let (res)
      (while (re-search-forward
              "^require +\\(?:['\"]\\)\\([^\n\"']+\\)" nil 'move)
        (push (match-string 1) res))
      res)))

;; insert require
(defun nvp-ruby-insert-require (lib)
  (save-excursion
    (goto-char (point-min))
    (forward-comment (point-max))
    (skip-syntax-backward " >")
    (insert (concat "\nrequire '" lib "'"))))

(defsubst nvp-ruby-require (lib)
  (unless (cl-member lib (nvp-ruby-requires) :test 'string=)
    (nvp-ruby-insert-require lib)))

;; ------------------------------------------------------------
;;; Commands

(defun nvp-ruby-start-robe ()
  (interactive)
  (robe-mode)
  (inf-ruby)
  (robe-start))

;; Compile
(defun nvp-ruby-compile ()
  (interactive)
  (ruby-compilation-this-buffer)
  (other-window 1))

;; Movement
(defsubst nvp-ruby-beginning-of-block ()
  (interactive)
  (ruby-end-of-block -1))

;; FIXME: Marking
(defsubst nvp-ruby-mark-block ()
  (interactive)
  (nvp--mark-defun))

;; code-folding
(eval-when-compile
  (require 'hideshow)
  (defmacro fold-show (&optional all)
    `(if (hs-already-hidden-p)
         (if ,all (hs-show-all) (hs-show-block))
       (if ,all (hs-hide-all) (hs-hide-block)))))

;; Fold classes/defs or just defs with prefix.
(defun nvp-ruby-fold-class/def (arg)
  (interactive "P")
  (nvp-with-hs-block "\\(?:def\\|class\\)" "\\(?:end\\)"
    (fold-show arg)))

(defun nvp-ruby-fold (arg)
  (interactive "P")
  (nvp-with-hs-block "\\(?:def\\)" "\\(?:end\\)"
    (fold-show arg)))

;; -------------------------------------------------------------------
;;; REPL

;; Save history and hippie expand from it
(nvp-hippie-shell-fn nvp-ruby-inf-setup ".irb_history")

(defun nvp-ruby-switch-to-repl (eob-p)
  (interactive "P")
  (if (buffer-live-p (bound-and-true-p inf-ruby-buffer))
      (ruby-switch-to-inf eob-p)
    (nvp-ruby-start-robe)))

(defun nvp-ruby-send-block ()
  (interactive)
  (condition-case nil
      (progn
        (ruby-send-block-and-go)
        (ruby-switch-to-last-ruby-buffer))
    (error
     (progn
       (ruby-send-definition-and-go)
       (ruby-switch-to-last-ruby-buffer)))))

(defun nvp-ruby-send-last-sexp-and-step ()
  (interactive)
  (end-of-line)
  (ruby-send-last-sexp)
  (forward-char 1))

(defun nvp-ruby-send-file ()
  (interactive)
  (save-buffer)
  (ruby-send-region-and-go
   (point-min)
   (point-max)))

;; -------------------------------------------------------------------
;;; Debug

;; insert breakpoint
(defun nvp-ruby-insert-breakpoint ()
  (nvp-ruby-require "pry")
  (insert "binding.pry"))

;; remove pry stuff
(defun nvp-ruby-remove-breakpoints ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "\\(^require ['\"]pry['\"]\\|binding\.pry\\)\\s-*" nil 'move)
      (replace-match "")
      (delete-blank-lines))))

;; insert breakpoint, with prefix remove all pry stuff
(defun nvp-ruby-breakpoint (arg)
  (interactive "P")
  (if arg (nvp-ruby-remove-breakpoints)
    (nvp-ruby-insert-breakpoint)))

;; ------------------------------------------------------------
;;; Tags

(defun nvp-ruby-update-rails-ctags ()
  "Update ctags for rails project."
  (interactive)
  (let ((default-directory
         (or (projectile-rails-root) default-directory)))
    (shell-command
     (concat "ctags -a -e -f "
             projectile-tags-file-name
             " --tag-relative -R app lib vender test"))))

;; -------------------------------------------------------------------
;;; Snippets

;; create arg initializtion from yas-text
;; (i,j) => @i = i\n@j = j
(defsubst nvp-ruby-yas-init-args ()
  (mapconcat (lambda (s) (concat "@" s " = " s))
             (split-string yas-text "[() ,]+" t " ")
             "\n"))

(defun nvp-ruby-yas-camelize-bfn ()
  (nvp-ruby-camelize (nvp-bfn 'no-ext)))

(provide 'nvp-ruby)

;;; Local Variables:
;;; lisp-indent-function: common-lisp-indent-function
;;; End:

;;; nvp-ruby.el ends here
