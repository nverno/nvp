;;; nvp-theme.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'moe-theme)
(require 'powerline)

(defun nvp-theme-reset (&optional revert)
  "Disable all active themes. With REVERT, revert mode-line."
  (interactive "P")
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (setq moe-theme-modeline-color 'blue
        moe-theme-highlight-buffer-id nil)
  (when revert
    (powerline-revert)))

(defun nvp-theme-light ()
  (setq moe-theme-modeline-color 'cyan
        moe-theme-highlight-buffer-id t)
  (moe-light)
  (powerline-moe-theme))

(defun nvp-theme-dark ()
  (load-theme 'gruvbox-dark-hard)
  (setq moe-theme-modeline-color 'blue
        moe-theme-highlight-buffer-id nil)
  ;; (moe-dark)
  (powerline-moe-theme))

;;;###autoload
(defun nvp-theme-switch (&optional arg)
  "Toggle light/dark themes. With ARG, remove all themes."
  (interactive "P")
  (nvp-theme-reset (nvp:prefix 16 'revert-mode-line))
  (unless arg
    (nvp:toggled-if (nvp-theme-dark)
      (nvp-theme-light))))

(provide 'nvp-theme)
;;; nvp-theme.el ends here
