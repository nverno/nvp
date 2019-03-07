;;; nvp-shell.el --- shell helpers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/shell-tools
;; Last modified: <2019-03-07 13:41:29>
;; Package-Requires: 
;; Created:  4 November 2016

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

;; [![Build Status](https://travis-ci.org/nverno/shell-tools.svg?branch=master)](https://travis-ci.org/nverno/shell-tools)

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))

(nvp-package-define-root :snippets t)

;; dont expand when prefixed by [-/_.]
(defvar nvp-shell-abbrev-re "\\(\\_<[_:\\.A-Za-z0-9/-]+\\)")

;; -------------------------------------------------------------------
;;; Utils

;; return some available shells
(defun nvp-shell-get-shells ()
  (nvp-with-gnu/w32 '("sh" "bash" "fish" "zsh")
    (cl-loop
       for var in `(,(expand-file-name "usr/bin" (getenv "MSYS_HOME"))
                    ,(expand-file-name "bin" (getenv "CYGWIN_HOME")))
       nconc (mapcar (lambda (x) (expand-file-name x var))
                     '("sh.exe" "bash.exe" "fish.exe" "zsh.exe")))))

;; hash table to hold bash aliases and their expansions
(defvar nvp-shell-alias-table nil)
(defun nvp-shell-alias-table ()
  (or nvp-shell-alias-table
      (prog1 (setq nvp-shell-alias-table (make-hash-table :size 129 :test 'equal))
        (dolist (line (process-lines "bash" "-ci" "alias"))
          (when (string-prefix-p "alias" line)
            (and (string-match "alias\\s-*\\([^=]+\\)=\\(.*\\)" line)
                 (puthash (match-string-no-properties 1 line)
                          ;; assume all aliases are of the form
                          ;; alias ..='cd ..'
                          ;; eg. the start and end with "'" that way don't have to
                          ;; worry about escaped single quotes when parsing
                          ;; aliases
                          (substring (match-string-no-properties 2 line) 1 -1)
                          nvp-shell-alias-table)))))))

;; get bash alias expansion, return nil if none
(defsubst nvp-shell-get-alias (alias)
  (gethash alias (nvp-shell-alias-table) nil))

;; -------------------------------------------------------------------
;;; Process

;; look for an active shell process
(defun nvp-shell-get-process (&optional proc-name buffer-name)
  (cl-loop for proc in (process-list)
     when (and (process-live-p proc)
               (cond
                (proc-name (string= (process-name proc) proc-name))
                (buffer-name (string= (buffer-name (process-buffer proc))
                                      buffer-name))
                (t (process-command proc)
                   (cl-find "-i" (process-command proc) :test 'string=))))
     return proc))

;; -------------------------------------------------------------------
;;; Commands

;; switch to a different shell for compiling
(defvar-local nvp-shell-current-shell "bash")
(defun nvp-shell-switch-shell (shell)
  "Switch to SHELL process."
  (interactive
   (list (if #'ido-completing-read
             (ido-completing-read "Shell: " (nvp-shell-get-shells))
           (completing-read "Shell: " (nvp-shell-get-shells)))))
  (setq nvp-shell-current-shell shell))

(defun nvp-shell-basic-compile ()
  "Run script with output to compilation buffer."
  (interactive)
  (let ((compile-command
         (concat (or nvp-shell-current-shell "bash") " "
                 (if buffer-file-name buffer-file-name)))
        (compilation-read-command))
    (call-interactively 'compile)))

(defun nvp-shell-expand-alias ()
  "Expand shell alias at/before point."
  (interactive)
  (skip-syntax-backward " " (comint-line-beginning-position))
  (unless (eq (point) (comint-line-beginning-position))
    (pcase-let* ((`(,start . ,end) (bounds-of-thing-at-point 'symbol))
                 (exp (nvp-shell-get-alias
                       (buffer-substring-no-properties start end))))
      (when exp
        (delete-region start end)
        (insert exp)))))

;; -------------------------------------------------------------------
;;; External

;; run input on current line in external shell (gnome) and give
;; it a name
(defun nvp-shell-run-external ()
  (interactive)
  (let ((proc (get-buffer-process (current-buffer)))
        ;; FIXME: inherit environment??
        )
    (if (not proc) (user-error "Current buffer has no process")
      (widen)
      (let* ((cmd (funcall comint-get-old-input))
             ;; FIXME: doesn't work -- how to pass env from gnome-shell => bash
             (process-environment
              (cons (format "PROMPT_COMMAND='echo -ne \"\\033]0;%s\\077\"'" cmd)
                    process-environment)))
        (and (not (string= "" (string-trim cmd)))
             (comint-send-string
              proc
              (format "gnome-terminal --tab -e \"bash -c '%s;bash'\"\n" cmd)))
        (comint-add-to-input-history cmd)
        (comint-delete-input)))))

(defun nvp-shell-nautilus ()
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (and proc (comint-send-string proc "nautilus . 2>/dev/null\n"))))

(provide 'nvp-shell)
;;; nvp-shell.el ends here
