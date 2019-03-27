;;; nvp-ext.el --- External programs -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Last modified: <2019-03-27 10:01:39>
;; URL: https://github.com/nverno/
;; Created: 11 November 2016

;;; Commentary:
;; - https://github.com/syohex/better-shell/blob/master/better-shell.el
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib)
  (nvp-local-vars)
  (defvar explicit-shell-file-name)
  (defvar epg-gpg-home-directory))
(require 'nvp-proc)
(declare-function imenu--make-index-alist "imenu")
(declare-function nvp-log "nvp-log")

;; FIXME: prompts for password
;; do sudo command and return process object
(defun nvp-ext-sudo-command (&optional password command buffer)
  (interactive
   (list (or (bound-and-true-p nvp-sudo-passwd)
             (nvp-lookup-password "localhost" (user-login-name) nil))
         (concat "'" (read-shell-command "Sudo command: ") "'")
         "*sudo-command*"))
  (let* ((default-directory "/sudo::")
         (proc (nvp-with-process "bash"
                 :proc-buff buffer
                 :proc-args ("bash -l" "-c" command)
                 :buffer-fn nvp-proc-comint-buffer
                 :shell t)))
    (sit-for 1)
    (process-send-string proc password)
    (process-send-string proc "\r")
    (process-send-eof proc)
    proc))

(defun nvp-ext-sudo-install (packages &optional buffer)
  (interactive (list (read-shell-command "Packages: ")))
  (nvp-ext-sudo-command
   nil
   (format "-l -c \"apt-get install -y %s\"" packages)
   buffer))

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
             "bash" (nvp-comint-buffer file) (concat "bash -l " cmd)))
        (start-process-shell-command
         "bash" (nvp-comint-buffer file) "bash -l " cmd)))))

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
                                     (or shell-name (getenv "SHELL"))))
         ;; always pop the shell in other window
         (display-buffer-overriding-action
          '(display-buffer-pop-up-window ((inhibit-same-window . t)))))
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
  (nvp-with-gnu (call-process "gnome-terminal" nil nil nil)))

;; -------------------------------------------------------------------
;;; Vagrant

;;;###autoload
(defun nvp-ext-vagrant-halt (&optional arg)
  "Halt all running vagrant boxes in `vms'.  With prefix, show output
in buffer *vagrant-status*."
  (interactive "P")
  (unless nvp/vms (user-error "'nvp/vms' is nil" nvp/vms))
  (nvp-with-process "bash"
    :proc-buff (and arg "*vagrant-status*")
    :proc-args ((expand-file-name "vms/vagrant-shizzle" nvp/bin) "-l" nvp/vms "-K"))
  (when (not arg)
    (message "Running vagrant-halt...")))

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
      :buffer-fn get-buffer-create
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

;;;###autoload
(defun nvp-ext-xev ()
  "Run xev with output to emacs buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*xev*")
    (pop-to-buffer (current-buffer))
    (local-set-key (kbd "C-c C-c") 'kill-this-buffer)
    (nvp-with-process "xev"
      :buffer-fn get-buffer-create
      :proc-filter nil
      :on-success (kill-buffer))))

(provide 'nvp-ext)
;;; nvp-ext.el ends here
