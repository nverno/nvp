;;; perl-w32tools ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/perl-w32tools
;; Package-Requires: 
;; Created:  8 November 2016

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

;; windows sucks

;;; Code:
(eval-when-compile
  (require 'cl-lib))
(require 'perl-tools)
(autoload 'nvp-env-rearrange-process-path "nvp-env")
(autoload 'nvp-env-rearrange-path "nvp-env")

;; ------------------------------------------------------------
;;; Environment

;; modes to set perl environment
(defvar perl-w32tools-perl-modes
  '(perl-mode cperl-mode inf-perl-mode pod-mode))

;; store original environment to change back
(defvar perl-w32tools--original-env nil)

;; Returns the process-environment rearranged with PATH entries
;; matching DIST or by default "strawberry" on windows.
;; This is so cpanm/perl finds the proper gcc, etc. when trying to
;; install stuff
(defun perl-w32tools-process-path (arg &optional dist no-case-fold)
  (if (not (eq system-type 'windows-nt))
      process-environment
    (nvp-env-rearrange-process-path
     (if arg
         (read-from-minibuffer "Perl distribution: " "strawberry")
       (or dist "strawberry"))
     (or no-case-fold t))))

;; setup up environment with proper perl at front of path
(defun perl-w32tools-setenv (&optional dist)
  (when (and (not perl-w32tools--original-env)
             (eq system-type 'windows-nt))
    ;; use strawberry perl by default unless DIST is specified
    (let ((path (nvp-env-rearrange-path (or dist "strawberry") t)))
      (setq perl-w32tools--original-env
            `((path . ,(copy-sequence (getenv "PATH")))
              (exec . ,(copy-sequence exec-path))))
      (setenv "PATH" (mapconcat 'identity path path-separator))
      (setq exec-path path))))

;; reset environment variables
(defun perl-w32tools-resetenv ()
  (when (eq system-type 'windows-nt)
    ;; restore PATH and `exec-path'
    (setenv "PATH" (cdr (assq 'path perl-w32tools--original-env)))
    (setq exec-path (cdr (assq 'exec perl-w32tools--original-env)))
    ;; reset so when opening another perl file env. will be
    ;; set again
    (setq perl-w32tools--original-env nil)))

;; Hook to run after done with perl environment:
;; when no more perl buffers found, reset environment
(defun perl-w32tools-cleanup-env ()
  (let ((this (current-buffer)))
    (unless (catch 'perl
              (mapc
               #'(lambda (buff)
                   (when (and (not (eq this buff))
                              (memq (buffer-local-value
                                     'major-mode (get-buffer buff))
                                    perl-w32tools-perl-modes))
                     (throw 'perl t)))
               (buffer-list))
              nil)
      ;; no more perl related buffers, reset environment
      (perl-w32tools-resetenv)
      (message "Environment variables reset."))))

;; ------------------------------------------------------------
;;; Debug

;; launch external shell on windows
(defun perl-w32tools-debug-shell (arg)
  (interactive "P")
  (let* ((default
           ;; default change to current directory then run debugger
           (format "/K %s && cd %s && perl -d %s"
                   (substring (file-truename default-directory) 0 2)
                   default-directory
                   (or buffer-file-name "")))
         (pars (if arg
                   (read-from-minibuffer "Parameters: " default)
                 default)))
    (w32-shell-execute "runas" "cmd.exe" pars)))

;; ------------------------------------------------------------

(declare-function w32-shell-execute "w32")

(provide 'perl-w32tools)
;;; perl-w32tools.el ends here
