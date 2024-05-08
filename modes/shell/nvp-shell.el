;;; nvp-shell.el --- shell helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'comint)

(nvp:decls :f (tramp-dissect-file-name
               nvp-comint-redirect-to-string shell-directory-tracker))
(nvp:auto "f" f-same-p)

;; dont expand when prefixed by [-/_.]
(defvar nvp-shell-abbrev-re "\\(\\_<[_:\\.A-Za-z0-9/-]+\\)")

;; update default-directory on remote login
(defvar nvp-shell-ssh-regexp (nvp:rx-syms "ssh" "hssh"))


;;; Processes

;; return some available shells
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
      ;; update current spot in history ring
      (setq comint-save-input-ring-index comint-input-ring-index)
      (setq comint-input-ring-index nil)
      cmd)))

;;; z directory tracking
(defun nvp-shell-z-resync ()
  (advice-remove #'shell-directory-tracker #'ignore)
  (let ((dir (nvp-comint-redirect-to-string "command dirs")))
    (when (file-exists-p dir)
      (shell-directory-tracker dir)
      (setq default-directory (file-name-as-directory dir)))))

(defun nvp-shell-z-tracker (str)
  (when (string-match-p "^\\s-*z\\b" str)
    (advice-add #'shell-directory-tracker :override #'ignore)
    (run-with-timer 0.4 nil #'nvp-shell-z-resync)))


;; -------------------------------------------------------------------
;;; Commands 
(defun nvp-shell-run-external ()
  "Run input on current line in external shell (gnome)"
  (interactive)
  (nvp:with-buffer-proc proc
    (-when-let (cmd (nvp-shell--get-input 'add))
      ;; FIXME: inherit environment?
      ;; this doesn't work -- how to pass env from gnome-shell => bash
      ;; (process-environment
      ;;  (cons (format "PROMPT_COMMAND='echo -ne \"\\033]0;%s\\077\"'" cmd)
      ;;        process-environment))
      (comint-send-string
       proc
       (format
        (concat "gnome-terminal --tab -- bash -c '"
                ;; "export PROMPT_COMMAND='echo -ne \"\\033]0;%s\\077\"';"
                "%s; bash'\n")
        cmd)))))

(defvar compilation-scroll-output)

(defun nvp-shell-compile (&optional arg)
  "Run current input in compilation buffer.
With prefix ARG, display in current buffer.
With `C-u'`C-u' read command."
  (interactive "P")
  (let ((display-buffer-overriding-action (and arg '((display-buffer-same-window)))))
    (-when-let (compile-command (nvp-shell--get-input 'add))
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
    (or (-when-let (root (and (bound-and-true-p projectile-mode)
                              (projectile-project-root)))
          (cl-some (lambda (buf)
                     (--> (projectile-project-root
                           (buffer-local-value 'default-directory buf))
                          (and (string= it root) buf)))
                   buffers))
        (car buffers))))

(defsubst nvp-display-buffer--height (alist &optional key)
  (when-let ((height (cdr (assq (or key 'window-height) alist))))
    (if (floatp height)
        (round (* height (frame-height)))
      height)))

;; modified `display-buffer-split-below-and-attach'
(defun nvp-display-buffer-split-below (buf alist)
  "Split current window and return bottom split."
  (let ((height (nvp-display-buffer--height alist)))
    (when height (setq height (- (max height window-min-height))))
    (window--display-buffer buf (split-window-below height) 'window alist)))

(defun nvp-display-buffer-below (buf alist)
  (let ((height (nvp-display-buffer--height alist 'window-min-height))
        (win (window-right (selected-window))))
    (while (and win (< (window-height win) height))
      (setq win (window-right win)))
    (when win
      (window--display-buffer buf win 'window alist))))

(defvar nvp-shell-display-buffer-default-action
  '((display-buffer-reuse-window nvp-display-buffer-split-below)
    (inhibit-same-window         . t)
    (window-height               . 0.5)
    (display-buffer-in-direction . left)))

;; 1. reuse window already displaying buffer
;; 2. split horizontally below if window large enough
;; 3. try to use window below, if one is large enough
;; 4. use current window
(defun nvp-shell--display-action (buffer)
  (let ((win (selected-window)))
    (cond ((eq (current-buffer) buffer)
           '(display-buffer-reuse-window))
          ((and (> (window-height win)
                   split-height-threshold)
                (window-splittable-p (selected-window)))
           nvp-shell-display-buffer-default-action)
          (t `((display-buffer-reuse-window
                nvp-display-buffer-below
                display-buffer-same-window)
               (reusable-frames   . visible)
               (window-min-height . ,(round (* 0.3 (frame-height)))))))))

(defun nvp-shell-display-buffer (buffer)
  (let ((display-buffer-overriding-action
         (nvp-shell--display-action buffer)))
    (shell buffer)))

;;;###autoload
(defun nvp-shell (arg &optional buffer shell-name proc-name)
  "Launch a shell using SHELL-NAME or env var SHELL, or bash if remote.
Use BUFFER if specified or create a unique remote name. If ARG, find or start
shell in current directory using PROC-NAME or default shell. When ARG isn't
specified, prefer shell in current directory if available."
  (interactive "P")
  (let* ((remote (file-remote-p default-directory))
         (default-name (if buffer (buffer-name buffer) "*shell*"))
         ;; explicit-shell-file-name
         (shell-file-name (if remote "/bin/bash"
                            (or shell-name (getenv "SHELL"))))
         (switch-to-buffer-obey-display-actions t)
         (split-height-threshold 60)
         (comint-terminfo-terminal "xterm-256color"))
    (if buffer (shell buffer)
      (if remote
          (shell (format "*shell:%s*"
                         (nth 2 (tramp-dissect-file-name default-directory))))
        ;; want a terminal in the current directory or project
        (let ((terms (nvp-shell-in-dir-maybe nil proc-name)))
          (if arg
              (let ((buffname (or (and terms (not (listp terms)) (buffer-name terms))
                                  ;; didn't find one -- create unique name
                                  (generate-new-buffer-name default-name))))
                ;; with double prefix, force new shell in current directory
                (nvp:prefix 16 (if (buffer-live-p (get-buffer buffname))
                                   (shell (generate-new-buffer-name default-name))
                                 (nvp-shell-display-buffer buffname))
                  (nvp-shell-display-buffer buffname)))
            ;; otherwise, any terminal will do, but prefer current directory
            ;; or project
            (nvp-shell-display-buffer (nvp-shell-in-project-maybe terms))))))))

;;;###autoload
(defun nvp-shell-launch-terminal ()
  "Launch external terminal."
  (interactive)
  (nvp:with-gnu (call-process "gnome-terminal" nil nil nil)))

(provide 'nvp-shell)
;;; nvp-shell.el ends here
