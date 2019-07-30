;;; nvp-shell.el --- shell helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; FIXME:
;; - reuse `nvp-shell-goto-command-start' for `sh-mode' as well
;; TODO: expand git aliases as well
;;; Code:
(eval-when-compile
  (require 'nvp-proc)
  (require 'nvp-macro)
  (require 'nvp-shell-macs "macs/nvp-shell-macs"))
(require 'comint)
(require 'nvp-shell-common)
(require 'nvp)
(nvp-decls)
(nvp-auto "f" f-same-p)

;; update default-directory on remote login
(defvar nvp-shell-ssh-regexp (nvp-re-opt '("ssh" "hssh")))

;; -------------------------------------------------------------------
;;; Things-at-point

;; guess bounds from beginning of current command to end of symbol/word at point
(defun nvp-shell-bounds-of-stmt-at-point ()
  (save-excursion
    (let* ((bol (comint-line-beginning-position))
           (end (progn (skip-syntax-forward "w_\"") (point)))
           (beg (progn (nvp-shell-goto-command-start end bol) (point))))
      (cons beg end))))
(put 'shell-stmt 'bounds-of-thing-at-point #'nvp-shell-bounds-of-stmt-at-point)

;; bounds for current active shell command
(defun nvp-shell-bounds-of-cmd-at-point ()
  (save-excursion
    (goto-char (car (bounds-of-thing-at-point 'shell-stmt)))
    (skip-syntax-forward "\"")
    (bounds-of-thing-at-point 'symbol)))
(put 'shell-cmd 'bounds-of-thing-at-point #'nvp-shell-bounds-of-cmd-at-point)

;; -------------------------------------------------------------------
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

;; -------------------------------------------------------------------
;;; Aliases

(defun nvp-shell-get-aliases (shell-cmd regex key val)
  "SHELL-CMD is a string passed to `call-process-shell-command' to print \
aliases. REGEX is used to match KEY VAL pairs that are added to a hash table."
  (let ((ht (make-hash-table :size 129 :test #'equal)))
    (with-temp-buffer
      (let ((standard-output (current-buffer)))
        (call-process-shell-command shell-cmd nil t nil)
        (goto-char (point-min))
        (while (re-search-forward regex nil 'move)
          (puthash (match-string-no-properties key)
                   (match-string-no-properties val) ht))
        ht))))

(nvp-define-cache-runonce nvp-shell-alias-table ()
  "Holds alist of hash tables mapping aliases to expansions.
Each cell is a cons (SYM . HASH)."
  ;; for bash assume all aliases are of the form
  ;; alias ..='cd ..'
  ;; eg. the start and end with "'" that way don't have to
  ;; worry about escaped single quotes when parsing
  ;; aliases
  (list (cons 'bash (nvp-shell-get-aliases
                     "bash -ci alias" "^alias\\s-*\\([^=]+\\)='\\(.*\\)'$" 1 2))
        (cons 'git (nvp-shell-get-aliases
                    "git alias" "^\\([^=]+\\)=\\(.+\\)$" 1 2))))

;; get alias expansion for key, return nil if none
(defsubst nvp-shell-get-alias (key alias)
  (gethash alias (cdr (assoc key (nvp-shell-alias-table))) nil))

;; -------------------------------------------------------------------
;;; Shell mode commands

;; TODO: expand git aliases as well
(defun nvp-shell-expand-alias ()
  "Expand shell alias at/before point."
  (interactive)
  (skip-syntax-backward " " (comint-line-beginning-position))
  (unless (eq (point) (comint-line-beginning-position))
    (pcase-let* ((`(,start . ,end) (bounds-of-thing-at-point 'symbol))
                 (exp (nvp-shell-get-alias
                       'bash
                       (buffer-substring-no-properties start end))))
      (when exp
        (delete-region start end)
        (insert exp)))))

(defun nvp-shell-run-external ()
  "Run input on current line in external shell (gnome)"
  (interactive)
  (nvp-with-proc proc
    ;; FIXME: inherit environment??
    (let* ((cmd (funcall comint-get-old-input))
           ;; FIXME: doesn't work -- how to pass env from gnome-shell => bash
           (process-environment
            (cons (format "PROMPT_COMMAND='echo -ne \"\\033]0;%s\\077\"'" cmd)
                  process-environment)))
      (and (not (string-blank-p cmd))
           (comint-send-string
            proc
            (format "gnome-terminal --tab -e \"bash -c '%s;bash'\"\n" cmd)))
      (comint-add-to-input-history cmd)
      (comint-delete-input))))

(defun nvp-shell-nautilus ()
  "Open nautilus in current directory."
  (interactive)
  (nvp-with-proc proc
    (comint-send-string proc "nautilus . 2>/dev/null\n")))

;; -------------------------------------------------------------------
;;; Remote

(defun nvp-shell-remote-filter (string)
  "Update `default-directory' on remote login.
Add to `comint-input-filter-functions'."
  (when (string-match-p nvp-shell-ssh-regexp string)
    (when-let* ((loc (comint-arguments string 1 1))
                (fullname (expand-file-name (concat "/ssh:" loc ":~/"))))
      (setq default-directory fullname
            list-buffers-directory fullname))))

(defun nvp-shell-tramp-name (&optional directory)
  (or directory (setq directory default-directory)))

;; -------------------------------------------------------------------
;;; Launch shells

;; Switch to a terminal or launch one, if remote use bash.
;; With prefix, create a shell in the current `default-directory'.
;; On remote hosts, ensure that the shell is created properly
;; (windows).
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
              (shell buffname))
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
