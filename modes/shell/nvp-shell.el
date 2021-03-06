;;; nvp-shell.el --- shell helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'comint)
(require 'nvp-shell-common)
(require 'nvp)
(nvp-req 'nvp-shell 'subrs)
(nvp-decls :f (tramp-dissect-file-name))
(nvp-auto "f" f-same-p)

;; update default-directory on remote login
(defvar nvp-shell-ssh-regexp (nvp-re-opt '("ssh" "hssh")))

;;; Processes

;; return some available shells
(nvp-define-cache-runonce nvp-shell-shells ()
  "List of possible shells."
  (nvp-with-gnu/w32 '("sh" "bash" "fish" "zsh" "csh")
    (cl-loop
       for var in `(,(expand-file-name "usr/bin" (getenv "MSYS_HOME"))
                    ,(expand-file-name "bin" (getenv "CYGWIN_HOME")))
       nconc (mapcar (lambda (x) (expand-file-name x var))
                     '("sh.exe" "bash.exe" "fish.exe" "zsh.exe")))))

;;; Commands

(defun nvp-shell--get-input (&optional add-history)
  (let ((cmd (funcall comint-get-old-input)))
    (unless (string-blank-p cmd)
      (and add-history (comint-add-to-input-history cmd))
      (comint-delete-input)
      ;; update current spot in history ring
      (setq comint-save-input-ring-index comint-input-ring-index)
      (setq comint-input-ring-index nil)
      cmd)))

(defun nvp-shell-run-external ()
  "Run input on current line in external shell (gnome)"
  (interactive)
  (nvp-with-proc proc
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

(defun nvp-shell-compile ()
  "Run current input in compilation buffer."
  (interactive)
  (nvp-with-proc proc
    (-when-let (compile-command (nvp-shell--get-input 'add))
      (call-interactively #'nvp-compile))))

(defun nvp-shell-nautilus ()
  "Open nautilus in current directory."
  (interactive)
  (nvp-with-proc proc
    (comint-send-string proc "nautilus . 2>/dev/null\n")))

;; -------------------------------------------------------------------
;;; Remote
;; TODO:
;; - use tramp in remote directory
;; - filter to update default-directory when ssh in shell

(defun nvp-shell-remote-filter (string)
  "Update `default-directory' on remote login.
Add to `comint-input-filter-functions'."
  (when (string-match-p nvp-shell-ssh-regexp string)
    (when-let* ((loc (comint-arguments string 1 1))
                (fullname (expand-file-name (concat "/ssh:" loc ":~/"))))
      (setq default-directory fullname
            list-buffers-directory fullname))))

(eval-when-compile
  (defsubst nvp-shell-tramp-name (&optional directory)
   (or directory (setq directory default-directory))))


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

;;;###autoload
(defun nvp-shell (arg &optional buffer shell-name proc-name)
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
            (let* ((terms (nvp-shell-in-dir-maybe nil proc-name))
                   (buffname (or (and terms (not (listp terms)) (buffer-name terms))
                                 ;; didn't find one -- create unique name
                                 (generate-new-buffer-name default-name))))
              ;; with double prefix, force new shell in current directory
              (nvp-prefix 16 (if (buffer-live-p (get-buffer buffname))
                                 (shell (generate-new-buffer-name default-name))
                               (shell buffname))
                (shell buffname)))
          ;; otherwise, any terminal will do, but prefer current directory
          (let ((terms (nvp-shell-in-dir-maybe nil proc-name)))
            (shell (or (and (listp terms) (car terms)) terms))))))))

;;;###autoload
(defun nvp-shell-launch-terminal ()
  "Launch external terminal."
  (interactive)
  (nvp-with-gnu (call-process "gnome-terminal" nil nil nil)))

(provide 'nvp-shell)
;;; nvp-shell.el ends here
