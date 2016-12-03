;;; nvp-ext --- -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/
;; Package-Requires: 
;; Created: 11 November 2016

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
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (defvar nvp/bin)
  (defvar nvp/vms)
  (defvar explicit-shell-file-name))
(autoload 'nvp-log "nvp-log")

(defmacro nvp-ext-read-passwd ()
  '(or (bound-and-true-p nvp-sudo-passwd)
       (read-passwd "Password: ")))

(nvp-with-gnu
  (defun nvp-ext-process-filter (proc string)
    (replace-regexp-in-string "\r+$" "" string)
    (with-current-buffer (process-buffer proc)
      (insert string)))
  
  ;; do sudo command and return process object
  (defun nvp-ext-sudo-command (&optional password command buffer)
    (interactive)
    (let* ((password (or password (nvp-ext-read-passwd)))
           (cmd (or command (read-shell-command "Command: ")))
           (proc (start-process-shell-command
                  "bash" (or buffer "*nvp-install*")
                  (concat "sudo bash -l " cmd))))
      (set-process-filter proc 'nvp-ext-process-filter)
      (process-send-string proc password)
      (process-send-string proc "\r")
      (process-send-eof proc)
      proc))

  (defun nvp-ext-sudo-install (packages &optional buffer)
    (interactive (list (read-shell-command "Packages: ")))
    (nvp-ext-sudo-command
     nil
     (format "-l -c \"apt-get install -y %s\"" packages)
     buffer)))

;;--- Terminal -------------------------------------------------------

(declare-function tramp-dissect-file-name "tramp")

;; Switch to a terminal or launch one, if remote use bash. 
;; With prefix, create a shell in the current `default-directory'. 
;; On remote hosts, ensure that the shell is created properly
;; (windows).

;;;###autoload
(defun nvp-ext-terminal (&optional buffer)
  (interactive)
  (let* ((prog "shell")
         (rem (file-remote-p default-directory))
         (explicit-shell-file-name
          (if rem "/bin/bash" (getenv "SHELL")))
         (buffname (or buffer
                       (concat "*" prog
                               (or (and rem
                                        (concat
                                         ":"
                                         (aref (tramp-dissect-file-name
                                                default-directory)
                                               2)))
                                   (and current-prefix-arg
                                        (concat
                                         ":"
                                         (substring
                                          default-directory
                                          0
                                          (1-
                                           (length
                                            default-directory))))))
                               "*")))
         (buffer (get-buffer-create buffname)))
    ;; (if (eq system-type 'windows-nt)
    ;;     (shell buffer)
    ;;   (ansi-term "/bin/bash" buffer))
    (shell buffer)))

;;--- Vagrant --------------------------------------------------------

;;;###autoload
(defun nvp-ext-vagrant-halt (&optional arg)
  "Halt all running vagrant boxes in `vms'.  With prefix, show output
in buffer *vagrant-status*."
  (interactive "P")
  (let* ((buff (if arg (get-buffer-create "*vagrant-status*") nil))
         (proc (start-process
                "bash" buff "bash"
                (expand-file-name "vagrant-tools.sh" nvp/bin) "-l"
                nvp/vms "-K")))
    (when (not arg)
      (message "Running vagrant-halt...")
      (nvp-with-process-log proc :pop-on-error))))

;;--- GPG ------------------------------------------------------------

;; export keys
;;;###autoload
(defun nvp-ext-gpg-export (dir)
  (interactive "DExport public key to directory: ")
  (unless (file-exists-p (nvp-program "gpg"))
    (user-error "gpg program not set"))
  (let ((default-directory dir))
    ;; export public key
    (nvp-with-process-log
      (start-process "gpg" (nvp-process-buffer) (nvp-program "gpg")
                     "--armor" "--output"
                     "public_key.txt" "--export" "Noah Peart"))))

;; copy gpg files to directory for backup/export
;;;###autoload
(defun nvp-ext-gpg-backup (dir)
  (interactive "DExport gpg files to directory: ")
  (unless (file-exists-p (nvp-program "gpg"))
    (user-error "gpg program not set."))
  (let ((default-directory epg-gpg-home-directory))
    (mapc (lambda (f)
            (copy-file f dir t))
          '("pubring.gpg" "secring.gpg" "trustdb.gpg"))))

;;--- Bash -----------------------------------------------------------

(nvp-with-w32
  ;; FIXME: move to w32-tools, also doesn't work
  (defun nvp-ext-bashw ()
    (interactive)
    (if current-prefix-arg
        (w32-shell-execute "runas" (nvp-program "bashw"))
      (let ((shell-file-name "bash")
            (explicit-shell-file-name (nvp-program "bashw"))
            (explicit-shell-args
             '("--login" "-i"))
            (w32-quote-process-args ?\")
            (buff "WindowsBash")
            (binary-process-input))
        (shell buff)))))

;;--- Other ----------------------------------------------------------

;; Return the number of logical processors on this system.
(defun nvp-ext-sys-numcores ()
  (or ;; Linux
   (when (file-exists-p "/proc/cpuinfo")
     (with-temp-buffer
       (insert-file-contents "/proc/cpuinfo")
       (how-many "^processor[[:space:]]+:")))
   ;; Windows
   (let ((number-of-processors (getenv "NUMBER_OF_PROCESSORS")))
     (when number-of-processors
       (string-to-number number-of-processors)))
   ;; BSD+OSX
   (with-temp-buffer
     (ignore-errors
       (when (zerop (call-process "sysctl" nil t nil "-n" "hw.ncpu"))
         (string-to-number (buffer-string)))))
   ;; Default
   1))

(provide 'nvp-ext)
;;; nvp-ext.el ends here
