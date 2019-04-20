;;; nvp-ext.el --- External programs -*- lexical-binding: t; -*-

;;; Commentary:
;; - https://github.com/syohex/better-shell/blob/master/better-shell.el
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro)
  (defvar epg-gpg-home-directory))
(require 'nvp-proc)
(nvp-decl nvp-lookup-password nvp-log)
(declare-function imenu--make-index-alist "imenu")

;; do sudo command and return process object
(defun nvp-ext-sudo-command (&optional password command buffer)
  (interactive
   (list (or (bound-and-true-p nvp-sudo-passwd)
             (nvp-lookup-password "localhost" (user-login-name) nil))
         (read-shell-command "Sudo command: ")
         "*sudo-command*"))
  (let* ((proc (nvp-with-process "bash"
                 :proc-buff buffer
                 :proc-args ("bash -l" "-c" command)
                 :buffer-fn nvp-proc-comint-buffer
                 :shell t)))
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
