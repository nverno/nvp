;;; nvp-theme.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'moe-theme)
(require 'powerline)

(defun nvp-theme-light ()
  (setq moe-theme-highlight-buffer-id t)
  (setq moe-theme-mode-line-color 'green)
  (moe-light)
  (powerline-moe-theme))

(defun nvp-theme-dark ()
  (load-theme 'gruvbox)
  (setq moe-theme-mode-line-color 'blue)
  (powerline-moe-theme))

;;;###autoload
(defun nvp-theme-switch ()
  "Toggle light/dark themes."
  (interactive)
  (nvp:toggled-if (nvp-theme-dark)
    (nvp-theme-light)))

(provide 'nvp-theme)
;;; nvp-theme.el ends here
