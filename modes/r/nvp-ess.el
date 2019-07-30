;;; nvp-ess.el --- ESS -*- lexical-binding: t; -*-
;;; Commentary:
;; stuff applicable across ESS dialects
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'ess)
(require 'ess-inf)
(require 'ess-utils)
(declare-function ess-mark-function-or-para "ess-mode")

;; Mark paragraphs, successively on repeated commands
;;;###autoload
(defun nvp-ess-mark-defun (&optional _arg)
  (interactive)
  (nvp--mark-defun
   ;; first mark
   (ess-mark-function-or-para)
   ;; repeated calls
   (condition-case nil
       (progn
         (forward-line 1)
         (end-of-defun))
     (error (forward-paragraph)))
   (point)))

;; -------------------------------------------------------------------
;;; Inferior

;;;###autoload
(defun nvp-ess-process-abort (arg)
  (interactive "P")
  (let ((proc (ess-get-process)))
    (when proc
      (if arg
          (kill-process proc)
        (interrupt-process proc)))))

;; Evaluate active region or line.
;;;###autoload
(defun nvp-ess-eval ()
  (interactive)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'ess-eval-region)
    (ess-eval-line-and-step)))

(provide 'nvp-ess)
;;; nvp-ess.el ends here
