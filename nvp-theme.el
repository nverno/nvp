;;; nvp-theme.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
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

;; toggle light/dark themes
;;;###autoload
(defun nvp-theme-switch ()
  (interactive)
  (if (not (eq last-command this-command))
      (nvp-theme-dark)
    (nvp-theme-light)
    (setq this-command nil)))

(provide 'nvp-theme)
;;; nvp-theme.el ends here
