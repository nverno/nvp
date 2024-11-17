;;; nvp-scratch.el --- simple minor mode for scratch -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-read)
(nvp:decls)

;;;###autoload
(defun nvp-jump-to-scratch (mode action &optional dir hookless)
  "Jump to scratch buffer in MODE (default current `major-mode').

With single extra prefix, keep current `default-directory' (default
\"~/scratch\" directory) or set to DIR.
With more extra prefixes, prompt for mode and optionally enable mode without
running its hook."
  (interactive (list major-mode current-prefix-arg))
  (nvp-with-display-actions action :action-order '(other same frame)
    (when current-prefix-arg
      (if (> (prefix-numeric-value current-prefix-arg) 4)
          (setq mode (intern (nvp-read-mode))
                hookless (y-or-n-p "Hookless? "))
        (setq dir (or dir default-directory))))
    (with-current-buffer (get-buffer-create "*scratch*")
      (setq default-directory (or dir nvp/scratch))
      (nvp-scratch-switch-modes mode (null hookless) hookless)
      (pop-to-buffer (current-buffer)))))

(defvar-local nvp-scratch--hookless nil
  "Non-nil when previous switch was hookless.")

(defun nvp-scratch-switch-modes (mode &optional activate hookless)
  "Switch major modes in scratch buffer.
With prefix, dont run modes hook."
  (interactive (list (intern (nvp-read-mode)) nil current-prefix-arg))
  (let ((mode (cond ((provided-mode-derived-p mode 'emacs-lisp-mode)
                     'lisp-interaction-mode)
                    ((provided-mode-derived-p mode 'comint-mode) 'sh-mode)
                    ;; Default when jumping to new scratch
                    (activate 'lisp-interaction-mode)
                    (t mode)))
        (start comment-start))
    (if hookless
        (let ((hook (intern (concat (symbol-name mode) "-hook"))))
          (kill-all-local-variables)
          (cl-progv (list hook) (list nil)
            (funcall mode)))
      (or (and (eq mode major-mode)
               (not nvp-scratch--hookless))
          (funcall mode)))
    (setq-local nvp-scratch--hookless hookless)
    (when (and comment-start
               (not (or (null start)
                        (equal start comment-start))))
      (replace-regexp-in-region
       (format "^\\s-*\\(?:%s+\\)+\\s-*"
               (regexp-quote (string-trim-right start)))
       (comment-padright comment-start (comment-add nil))
       (point-min) (point-max)))
    (setq header-line-format
          `("• "
            (:eval (propertize (abbreviate-file-name default-directory)
                               'face 'font-lock-variable-name-face))
            ,(concat " (" (propertize (symbol-name mode) 'face 'italic) ")")
            ,(when hookless
               (propertize " [hookless]" 'face 'font-lock-keyword-face)))))
  (nvp-scratch-minor-mode 1))

(defun nvp-scratch-kill-buffer ()
  "Kill buffer ignoring `kill-buffer-query-functions'."
  (interactive)
  (let (kill-buffer-hook kill-buffer-query-functions)
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
    (nvp:msg "Press \\<nvp-scratch-minor-mode-map>\\[nvp-scratch-kill-buffer] to kill \
this buffer or \\<nvp-scratch-minor-mode-map>\\[nvp-scratch-switch-modes] \
to switch major modes.")))

(provide 'nvp-scratch)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-scratch.el ends here
