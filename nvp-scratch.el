;;; nvp-scratch.el --- simple minor mode for scratch -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'nvp-read)
(nvp:auto "nvp-yas" nvp-comment-string)

(defun nvp-scratch-switch-modes (mode &optional activate)
  "Switch major modes in scratch buffer."
  (interactive (list (intern (nvp-read-mode))))
  (cond ((provided-mode-derived-p mode 'emacs-lisp-mode)
         (setq mode 'lisp-interaction-mode))
        ((provided-mode-derived-p mode 'comint-mode)
         (setq mode 'sh-mode))
        (t (and activate                ; default when jumping to new scratch
                (setq mode 'lisp-interaction-mode))))
  (let ((start comment-start))
    (or (eq mode major-mode) (funcall mode))
    (unless (or (null start)
                (equal start comment-start))
      (replace-regexp-in-region
       (format "^\\s-*\\(?:%s\\)+\\s-*" (regexp-quote (string-trim-right start)))
       (concat (string-trim comment-start) " ")
       (point-min) (point-max))))
  (or (bound-and-true-p nvp-scratch-minor-mode) (nvp-scratch-minor-mode)))

(defun nvp-scratch-kill-buffer ()
  "Kill buffer ignoring `kill-buffer-query-functions'."
  (interactive)
  (let ((kill-buffer-hook '(nvp-window-configuration-restore))
        kill-buffer-query-functions)
    (kill-buffer (current-buffer))))

(eval-and-compile
  (defvar-keymap nvp-scratch-minor-mode-map
    "C-c C-k" #'nvp-scratch-kill-buffer
    "C-c C-s" #'nvp-scratch-switch-modes))

;;;###autoload
(define-minor-mode nvp-scratch-minor-mode
  "Minor mode in scratch buffers."
  :lighter " 𝓢"
  (when nvp-scratch-minor-mode
    (setq-local kill-buffer-hook '(nvp-window-configuration-restore))
    (when (eq (point-min) (point-max))
      (let ((comment-start (or comment-start "#")))
        (insert (nvp-comment-string "Jah lives chilren\n" 2))))
    (nvp:msg "Press \\<nvp-scratch-minor-mode-map>\\[nvp-scratch-kill-buffer] to kill \
this buffer or \\<nvp-scratch-minor-mode-map>\\[nvp-scratch-switch-modes] \
to switch major modes.")))

(provide 'nvp-scratch)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-scratch.el ends here
