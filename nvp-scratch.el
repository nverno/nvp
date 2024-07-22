;;; nvp-scratch.el --- simple minor mode for scratch -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'nvp-read)
(nvp:auto "nvp-yas" nvp-comment-string)

;;;###autoload
(defun nvp-jump-to-scratch (mode action &optional hookless)
  "Jump to scratch buffer in MODE (default current `major-mode').
With prefix, pop other window, with double prefix, prompt for MODE."
  (interactive
   (let* ((prompt-p (>= (prefix-numeric-value current-prefix-arg) 16))
          (mode (if prompt-p (intern (nvp-read-mode)) major-mode))
          (hookless (and prompt-p (y-or-n-p "Hookless!? "))))
     (list mode current-prefix-arg hookless)))
  (nvp-window-configuration-save)
  (let ((buff (get-buffer-create "*scratch*")))
    (with-current-buffer buff
      (setq default-directory nvp/scratch)
      (nvp-scratch-switch-modes mode (null hookless) hookless)
      (nvp-display-location buff :buffer action))))

(defvar-local nvp-scratch--hookless nil
  "Non-nil when previous switch was hookless.")

(defun nvp-scratch-switch-modes (mode &optional activate hookless)
  "Switch major modes in scratch buffer.
With prefix, dont run modes hook."
  (interactive (list (intern (nvp-read-mode)) nil current-prefix-arg))
  (cond ((provided-mode-derived-p mode 'emacs-lisp-mode)
         (setq mode 'lisp-interaction-mode))
        ((provided-mode-derived-p mode 'comint-mode)
         (setq mode 'sh-mode))
        (t (and activate                ; default when jumping to new scratch
                (setq mode 'lisp-interaction-mode))))
  (let ((start comment-start))
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
          (concat (if hookless "[hookless] " "") (symbol-name mode))))
  (or (bound-and-true-p nvp-scratch-minor-mode)
      (nvp-scratch-minor-mode)))

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
    (nvp:msg "Press \\<nvp-scratch-minor-mode-map>\\[nvp-scratch-kill-buffer] to kill \
this buffer or \\<nvp-scratch-minor-mode-map>\\[nvp-scratch-switch-modes] \
to switch major modes.")))

(provide 'nvp-scratch)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-scratch.el ends here
