;;; From nvp-ext -- no longer used

;; do sudo command and return process object
(defun nvp-ext-sudo-command (&optional password command buffer)
  (interactive
   (list (or (bound-and-true-p nvp-sudo-passwd)
             (nvp-lookup-password "localhost" (user-login-name) nil))
         (read-shell-command "Sudo command: ")
         "*sudo-command*"))
  (let* ((proc (nvp-with-process "bash"
                 :proc-buff (nvp-comint-buffer :name buffer :new t)
                 :proc-args ("-l" "-c" command)
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
    (mapcar #'car (cdr (imenu--make-index-alist)))))

;; create command to run. If function is cons, first element is function and the
;; rest are its args
(defun nvp-ext--script-command (file &optional functions)
  (let ((funcs (mapcar (lambda (f) (if (listp f)
                                  (mapconcat #'identity f " ")
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
             "bash" (nvp-comint-buffer :name file) (concat "bash -l " cmd)))
        (start-process-shell-command
         "bash" (nvp-comint-buffer :name file) "bash -l " cmd)))))

