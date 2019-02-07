;;; nvp-ext.el --- External programs -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Last modified: <2019-02-06 19:04:34>
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
  (nvp-local-vars)
  (defvar explicit-shell-file-name)
  (defvar epg-gpg-home-directory))
(declare-function imenu--make-index-alist "imenu")
(autoload 'nvp-log "nvp-log")
(autoload 'nvp-process-buffer "nvp")

;; -------------------------------------------------------------------
;;; Sudo

(defmacro nvp-ext-read-passwd ()
  '(or (bound-and-true-p nvp-sudo-passwd)
       (read-passwd "Password: ")))

(nvp-with-gnu
  (defun nvp-ext-process-filter (proc string)
    (replace-regexp-in-string "[\n\r]+" "\n" string)
    (with-current-buffer (process-buffer proc)
      (insert string)))
  
  ;; do sudo command and return process object
  (defun nvp-ext-sudo-command (&optional password command buffer)
    (interactive)
    (let* ((password (or password (nvp-ext-read-passwd)))
           (cmd (or command (read-shell-command "Command: ")))
           (proc (start-process-shell-command
                  "bash" (or buffer (nvp-process-buffer 'comint))
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

;; -------------------------------------------------------------------
;;; Bash Script

(defun nvp-ext--script-functions (file)
  (with-current-buffer (find-file-noselect file)
    (mapcar 'car (cdr (imenu--make-index-alist)))))

;; create command to run. If function is cons, first element is function and the
;; rest are its args
(defun nvp-ext--script-command (file &optional functions)
  (let ((funcs (mapcar (lambda (f) (if (listp f)
                                  (mapconcat 'identity f " ")
                                f))
                       functions)))
    (if functions
        (mapconcat (lambda (f) (concat file " " f)) funcs " && ")
      file)))

;; run bash script. If FUNCTIONS is non-nil call those functions
;; from script, as SUDO if non-nil.
;; Interactively, prompts for file and functions
;;;###autoload
(defun nvp-ext-run-script (file &optional functions sudo passwd)
  (interactive
   (let* ((file (read-file-name "File: "))
          (funcs (cons (ido-completing-read
                        "Function: "
                        (nvp-ext--script-functions file))
                       nil))
          (sudo (nvp-with-gnu (y-or-n-p "Sudo? "))))
     (list file funcs sudo nil)))
  (let ((cmd (nvp-ext--script-command file functions)))
    (nvp-with-process-filter
      (nvp-with-gnu/w32
          (if sudo
              (nvp-ext-sudo-command passwd cmd)
            (start-process-shell-command
             "bash" (nvp-process-buffer 'comint) (concat "bash -l " cmd)))
        (start-process-shell-command
         "bash" (nvp-process-buffer 'comint) "bash -l " cmd)))))

;;;###autoload
(define-obsolete-function-alias 'nvp-install-script 'nvp-ext-run-script)
;;;###autoload
(define-obsolete-function-alias 'nvp-install-script-functions
  'nvp-ext--script-functions)

;; -------------------------------------------------------------------
;;; Terminal

(declare-function tramp-dissect-file-name "tramp")

;; Switch to a terminal or launch one, if remote use bash.
;; With prefix, create a shell in the current `default-directory'.
;; On remote hosts, ensure that the shell is created properly
;; (windows).
(defun nvp-ext-terminal-in-dir-maybe (&optional directory proc-name)
  "Return a terminal buffer running in DIRECTORY (default-directory by default).
If none found, return list of all terminal buffers."
  (setq proc-name (or proc-name "shell"))
  (setq directory (file-name-directory (or directory default-directory)))
  (cl-loop for proc in (process-list)
     when (and (string-prefix-p proc-name (process-name proc))
               (process-live-p proc))
     if (and (with-current-buffer (process-buffer proc)
               (string= directory default-directory)))
     return (process-buffer proc)
     else collect (process-buffer proc)))

(defun nvp-ext-find-terminal (&optional proc-name)
  "Return the first running terminal buffer that may be under a weird buffer name."
  (setq proc-name (or proc-name "shell"))
  (cl-loop for proc in (process-list)
     when (and (string-prefix-p proc-name (process-name proc))
               (process-live-p proc))
     return proc))

(defun nvp-ext-all-terminals (&optional proc-name)
  "Collect list of all terminal buffers."
  (setq proc-name (or proc-name "shell"))
  (cl-loop for proc in (process-list)
     if (and (string-prefix-p proc-name (process-name proc))
             (process-live-p proc))
     collect (process-buffer proc)))

;;;###autoload
(defun nvp-ext-terminal (arg &optional buffer shell-name proc-name)
  "Launch a shell using SHELL-NAME or env var SHELL, or bash if remote.
Use BUFFER if specified or create a unique remote name. If ARG, find or start
shell in current directory using PROC-NAME or default shell. When ARG isn't
specified, prefer shell in current directory if available."
  (interactive "P")
  (let* ((remote (file-remote-p default-directory))
         (default-name (if buffer (buffer-name buffer) "*shell*"))
         (explicit-shell-file-name (if remote "/bin/bash"
                                     (or shell-name (getenv "SHELL")))))
    (if buffer (shell buffer)
      (if remote
          (shell (format "*shell:%s*"
                         (nth 2 (tramp-dissect-file-name default-directory))))
        ;; want a terminal in the current directory
        (if arg
            (let* ((terms (nvp-ext-terminal-in-dir-maybe nil proc-name))
                   (buffname (or (and terms (not (listp terms)) (buffer-name terms))
                                 ;; didn't find one -- create unique name
                                 (generate-new-buffer-name default-name))))
              (shell buffname))
          ;; otherwise, any terminal will do, but prefer current directory
          (let ((terms (nvp-ext-terminal-in-dir-maybe nil proc-name)))
            (shell (or (and (listp terms) (car terms)) terms))))))))

;;;###autoload
(defun nvp-ext-launch-terminal ()
  "Launch external terminal."
  (interactive)
  (nvp-with-gnu
    (call-process "gnome-terminal" nil nil nil)))

;; -------------------------------------------------------------------
;;; Vagrant

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

;; -------------------------------------------------------------------
;;; GPG

;;;###autoload
(defun nvp-ext-gpg-export (dir &optional name prog)
  "Export GPG public keys matching NAME (default NVP) to DIR using PROG (gpg/2)."
  (interactive "DExport public key to directory: \nsKey name (NVP): ")
  (or prog (setq prog (or (nvp-program "gpg2") (nvp-program "gpg"))))
  (and (equal name "") (setq name "NVP"))
  (unless (and prog (file-exists-p prog))
    (error (if prog "%s not found" "No gpg found") prog))
  (let ((default-directory dir))
    (nvp-with-process prog
      :proc-name "gpg"
      :get-buff-function get-buffer-create
      :proc-args ("--armor" "--output" "public_key.asc" "--export" name))))

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

;; -------------------------------------------------------------------
;;; Other

(defun nvp-ext-start-process (cmd)
  (start-process
   cmd nil shell-file-name
   shell-command-switch
   (format "nohup 1>/dev/null 2>/dev/null %s" cmd)))

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
