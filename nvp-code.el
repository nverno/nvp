;;; nvp-code.el ---  -*- lexical-binding: t; -*-

;;; Commentary:

;; - code folding
;; - hi-lock
;; - narrow

;;; Code:
(eval-when-compile
  (require 'nvp-macro))
(nvp-declare hs-toggle-hiding hs-show-all hs-hide-all)

;;; Hideshow

(defvar hs-minor-mode)
(defvar hs-set-up-overlay)

(defvar-local nvp-hs--hidden nil)

(defun nvp-hs-display-line-counts (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put ov 'face 'font-lock-comment-face)
    (overlay-put ov 'display
                 (format " ... %d lines"
                         (count-lines (overlay-start ov)
                                      (overlay-end ov))))))

;;;###autoload
(defun nvp-hs-toggle (&optional arg)
  "Toggle hideshow on block.  With prefix toggle all."
  (interactive "P")
  (unless (bound-and-true-p hs-minor-mode)
    (hs-minor-mode))
  (if arg
      (if (setq nvp-hs--hidden (not nvp-hs--hidden))
          (hs-show-all)
        (hs-hide-all))
    (hs-toggle-hiding))
  (nvp-repeat-command nil nil nil 1))

(with-eval-after-load "hideshow"
  (unless (not (eq 'ignore hs-set-up-overlay))
    (setq hs-set-up-overlay 'nvp-hs-display-line-counts)))

;;;;;###autoload
(add-hook 'hs-minor-mode-hook
          (nvp-def nvp-hs-mode-hook ()
            (unless hs-set-up-overlay
              (setq hs-set-up-overlay 'nvp-hs-display-line-counts))))

(provide 'nvp-code)
;;; nvp-code.el ends here
