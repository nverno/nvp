;;; nvp-ido.el ---  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'ido)
(nvp-decls)

(defun nvp-ido-refresh-homedir ()
  "Refresh completion for homedir while ido is finding a file."
  (interactive)
  (ido-set-current-directory "~/")
  (setq ido-exit 'refresh)
  (exit-minibuffer))

(defun nvp-ido-yank ()
  "Forward to `yank'."
  (interactive)
  (if (file-exists-p (current-kill 0))
      (ido-fallback-command)
    (yank)))

(defun nvp-ido-backspace ()
  "Forward to `backward-delete-char'.
On error (read-only), quit without selecting."
  (interactive)
  (condition-case nil
      (backward-delete-char 1)
    (error
     (minibuffer-keyboard-quit))))

(defun nvp-ido-beginning-of-input ()
  (interactive)
  (goto-char (minibuffer-prompt-end)))

;; return default-directory on `ido-fallback-command'
(defun nvp-ido-fallback (&rest _)
  (interactive)
  (setq ido-text (or (file-name-directory ido-text) ""))
  (setq ido-exit 'done)
  (exit-minibuffer))

(defun nvp-ido-throw-dired ()
  (interactive)
  (throw 'dired t))

(provide 'nvp-ido)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-ido.el ends here
