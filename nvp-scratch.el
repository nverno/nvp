;;; nvp-scratch.el --- simple minor mode for scratch -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-24 20:28:18>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 24 March 2019

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'nvp)
(require 'nvp-read)

(defun nvp-scratch-switch-modes (mode)
  "Switch major modes in scratch buffer."
  (interactive (list (intern (nvp-read-mode))))
  (cond
   ((provided-mode-derived-p mode 'emacs-lisp-mode)
    (setq mode 'lisp-interaction-mode))
   ((provided-mode-derived-p mode 'comint-mode)
    (setq mode 'sh-mode)))
  (funcall mode)
  (nvp-scratch-minor-mode))

(defun nvp-scratch-kill-buffer ()
  "Kill buffer ignoring `kill-buffer-hook', `kill-buffer-query-functions'."
  (interactive)
  (let (kill-buffer-hook kill-buffer-query-functions)
    (kill-this-buffer))
  (nvp-window-configuration-restore))

(defvar nvp-scratch-minor-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "C-c C-k") #'nvp-scratch-kill-buffer)
    (define-key km (kbd "C-c C-s") #'nvp-scratch-switch-modes)
    km))

;;;###autoload
(define-minor-mode nvp-scratch-minor-mode
  "Minor mode in scratch buffers."
  :lighter nil
  (when nvp-scratch-minor-mode
    (add-hook 'after-change-major-mode-hook #'nvp-scratch-minor-mode nil 'local)
    (erase-buffer)
    (insert (nvp-comment-string "Jah lives chilren\n" 2))
    (setq mode-name (concat "scratch[" mode-name "]"))
    (nvp-msg "Press \\[nvp-scratch-kill-buffer] to kill this buffer \
or \\[nvp-scratch-switch-modes] to switch major modes. " :keys t)))

(provide 'nvp-scratch)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-scratch.el ends here
