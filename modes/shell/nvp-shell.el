;;; nvp-shell.el --- shell helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'comint)
(nvp:decls :p (shell tramp) :f (nvp-comint-redirect-to-string))

(autoload 'f-same-p "f")


(nvp:define-cache-runonce nvp-shell-shells ()
  "List of possible shells."
  (nvp:with-gnu/w32 '("sh" "bash" "fish" "zsh" "csh")
    (cl-loop
     for var in `(,(expand-file-name "usr/bin" (getenv "MSYS_HOME"))
                  ,(expand-file-name "bin" (getenv "CYGWIN_HOME")))
     nconc (mapcar (lambda (x) (expand-file-name x var))
                   '("sh.exe" "bash.exe" "fish.exe" "zsh.exe")))))

(defun nvp-shell--get-input (&optional add-history)
  (let ((cmd (funcall comint-get-old-input)))
    (unless (string-blank-p cmd)
      (and add-history (comint-add-to-input-history cmd))
      (comint-delete-input)
      ;; Update current spot in history ring
      (setq comint-save-input-ring-index comint-input-ring-index)
      (setq comint-input-ring-index nil)
      cmd)))


;;; Z directory tracking

(defun nvp-shell-resync-dirs (&optional dirtrack)
  "Replacement for `shell-resync-dirs' to handle multiline prompt."
  (interactive (list t))
  (let ((dir (nvp-comint-redirect-to-string "command dirs")))
    (when (file-exists-p dir)
      (setq default-directory (file-name-as-directory dir))
      (setq list-buffers-directory default-directory)
      (when dirtrack
        (with-demoted-errors "Couldn't cd: %s"
          (shell-cd default-directory)
          (setq shell-dirstack nil
                shell-last-dir default-directory)
          (shell-dirstack-message)))
      dir)))

(defun nvp-shell-z-resync ()
  (advice-remove #'shell-directory-tracker #'ignore)
  (nvp-shell-resync-dirs))

(defun nvp-shell-z-tracker (str)
  (when (string-match-p "^\\s-*z\\b" str)
    (advice-add #'shell-directory-tracker :override #'ignore)
    (run-with-timer 0.2 nil #'nvp-shell-z-resync)))


;; -------------------------------------------------------------------
;;; Commands

(defun nvp-shell-run-external ()
  "Run input on current line in external shell (gnome)"
  (interactive)
  (nvp:with-buffer-proc proc
    (when-let* ((cmd (nvp-shell--get-input 'add)))
      ;; FIXME: inherit environment?
      ;; this doesn't work -- how to pass env from gnome-shell => bash
      ;; (process-environment
      ;;  (cons (format "PROMPT_COMMAND='echo -ne \"\\033]0;%s\\077\"'" cmd)
      ;;        process-environment))
      (comint-send-string
       proc (format (concat
                     "gnome-terminal --tab -- bash -c '"
                     ;; "export PROMPT_COMMAND='echo -ne \"\\033]0;%s\\077\"';"
                     "%s; bash'\n")
                    cmd)))))

(defvar compilation-scroll-output)

(defun nvp-shell-compile (&optional arg)
  "Run current input in compilation buffer.
With prefix \\[universal-argument] ARG, display in current buffer.
With \\[universal-argument] \\[universal-argument], read command."
  (interactive "P")
  (let ((display-buffer-overriding-action
         (and arg (nvp-display-buffer-action 'same-window))))
    (when-let* ((compile-command (nvp-shell--get-input 'add)))
      (nvp:with-global-vars
          ((compilation-read-command (>= (prefix-numeric-value arg) 16))
           (compilation-scroll-output nil))
        (setq current-prefix-arg compilation-read-command)
        (with-current-buffer (call-interactively #'nvp-compile)
          (goto-char (point)))))))

(defun nvp-shell-nautilus ()
  "Open nautilus in current directory."
  (interactive)
  (nvp:with-buffer-proc proc
    (comint-send-string proc "nautilus . 2>/dev/null\n")))


;; -------------------------------------------------------------------
;;; Remote

;; TODO:
;; - use tramp in remote directory
;; - filter to update default-directory when ssh in shell

;; update default-directory on remote login
(defvar nvp-shell-ssh-regexp (nvp:rx-syms "ssh" "hssh"))

;; (defun nvp-shell-remote-filter (string)
;;   "Update `default-directory' on remote login.
;; Add to `comint-input-filter-functions'."
;;   (when (string-match-p nvp-shell-ssh-regexp string)
;;     (when-let* ((loc (comint-arguments string 1 1))
;;                 (fullname (expand-file-name (concat "/ssh:" loc ":~/"))))
;;       (setq default-directory fullname
;;             list-buffers-directory fullname))))

;; (eval-when-compile
;;   (defsubst nvp-shell-tramp-name (&optional directory)
;;     (or directory (setq directory default-directory))))


;; -------------------------------------------------------------------
;;; Launch shells
;;
;; Switch to a terminal or launch one, if remote use bash.
;; With prefix, create or get a shell in the current `default-directory'.
;; On remote hosts, ensure that the shell is created properly (windows).

(defun nvp-shell-in-dir-maybe (&optional directory proc-name)
  "Return a terminal buffer running in DIRECTORY (default-directory by default).
If none found, return list of all terminal buffers."
  (setq proc-name (or proc-name "shell"))
  (setq directory (file-name-directory (or directory default-directory)))
  (cl-loop for proc in (process-list)
           as pbuff = (process-buffer proc)
           when (and (string-prefix-p proc-name (process-name proc))
                     (process-live-p proc))
           if (f-same-p directory (buffer-local-value 'default-directory pbuff))
           return pbuff
           else collect pbuff))

(defun nvp-shell-in-project-maybe (buffers)
  "Return buffer in current project if there is one in BUFFERS."
  (if (not (listp buffers)) buffers
    (or (when-let* ((root (and (bound-and-true-p projectile-mode)
                               (projectile-project-root))))
          (cl-some (lambda (buf)
                     (--> (projectile-project-root
                           (buffer-local-value 'default-directory buf))
                          (and (string= it root) buf)))
                   buffers))
        (car buffers))))


(defun nvp-shell--display-maybe-other-window (buf alist)
  (when (window-full-width-p)
    (display-buffer-pop-up-window buf alist)))

(defvar nvp-shell-display-buffer-action
  `((display-buffer-reuse-window
     display-buffer-reuse-mode-window
     nvp-shell--display-maybe-other-window
     display-buffer-below-selected
     display-buffer-in-direction
     display-buffer-same-window)
    (mode shell-mode)
    (category          . repl)
    (direction         . below)
    (window-min-height . 0.35)
    (window-height     . 0.5)
    (preserve-size . (t . t)))
  "Display action for `nvp-shell'.")

;;;###autoload
(defun nvp-shell (arg &optional buffer shell proc-name)
  "Launch a shell using SHELL, envvar \"SHELL\", or \"/bin/bash\" when remote.

Use BUFFER if specified or create a unique remote name. New shells are
created with PROC-NAME or the default shell.

ARG determines when and where shells are created:

  \\[universal-argument]     Get or create shell in current directory or project.
  \\[universal-argument]\\[universal-argument]  Definetly create new shell in
  current directory.

With no ARG, prefer shells in current directory or project when available."
  (interactive "P")
  (let* ((remote (file-remote-p default-directory))
         (default-name (if buffer (buffer-name buffer) "*shell*"))
         ;; `explicit-shell-file-name'
         (shell-file-name (if remote "/bin/bash"
                            (or shell (getenv "SHELL"))))
         (switch-to-buffer-obey-display-actions t)
         (split-height-threshold 60)
         (comint-terminfo-terminal "xterm-256color")
         (force-new (eq 16 (prefix-numeric-value current-prefix-arg)))
         (bufname
          (cond
           (buffer buffer)
           (remote (format "*shell:%s*" (nth 2 (tramp-dissect-file-name
                                                default-directory))))
           (t (let ((terms (nvp-shell-in-dir-maybe nil proc-name)))
                (if arg
                    (let ((name (or (and terms (not (listp terms))
                                         (buffer-name terms))
                                    ;; Didn't find one -- create unique name
                                    (generate-new-buffer-name default-name))))
                      ;; With double prefix, force new shell in current
                      ;; directory
                      (if (and force-new
                               (buffer-live-p (get-buffer name)))
                          (generate-new-buffer-name default-name)
                        name))
                  ;; Otherwise, any terminal will do, but prefer current
                  ;; directory or project
                  (nvp-shell-in-project-maybe terms)))))))
    (pop-to-buffer
     (or (and bufname (get-buffer bufname))
         (shell bufname))
     nvp-shell-display-buffer-action)))

;;;###autoload
(defun nvp-shell-launch-terminal ()
  "Launch external terminal."
  (interactive)
  (call-process "gnome-terminal" nil nil nil))

(provide 'nvp-shell)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-shell.el ends here
