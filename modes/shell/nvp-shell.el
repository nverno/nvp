;;; nvp-shell.el --- shell helpers -*- lexical-binding: t; -*-

;; Last modified: <2019-03-09 06:18:40>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created:  4 November 2016

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(require 'nvp)

;; dont expand when prefixed by [-/_.]
(defvar nvp-shell-abbrev-re "\\(\\_<[_:\\.A-Za-z0-9/-]+\\)")

;; -------------------------------------------------------------------
;;; Utils

;; return some available shells
(defun nvp-shell-get-shells ()
  (nvp-with-gnu/w32 '("sh" "bash" "fish" "zsh" "csh")
    (cl-loop
       for var in `(,(expand-file-name "usr/bin" (getenv "MSYS_HOME"))
                    ,(expand-file-name "bin" (getenv "CYGWIN_HOME")))
       nconc (mapcar (lambda (x) (expand-file-name x var))
                     '("sh.exe" "bash.exe" "fish.exe" "zsh.exe")))))

;; FIXME: Isn't something similar defined elsewhere?
;; look for an active interactive shell process
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
;;; Commands

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
