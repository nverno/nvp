;;; nvp-compilation.el --- compilation helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro))
(require 'compile)
(nvp-decl comint-after-pmark-p)

;; move to next warning/error 
(defun nvp-compilation-next (n)
  "Move to next warning, error, or info message."
  (interactive "p")
  (let ((compilation-skip-threshold 0))
    (compilation-next-error n)))

(defun nvp-compilation-previous (n)
  (interactive "p")
  (nvp-compilation-next (- n)))

;; -------------------------------------------------------------------
;;; compilation-shell-minor-mode 

(defun nvp-compilation-next-or-complete (n)
  "Unless after comint prompt, move to Nth next error, otherwise complete."
  (interactive "p")
  (if (comint-after-pmark-p)
      (completion-at-point)
    (nvp-compilation-next n)))


(provide 'nvp-compilation)
;;; nvp-compilation.el ends here
