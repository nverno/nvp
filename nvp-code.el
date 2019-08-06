;;; nvp-code.el ---  -*- lexical-binding: t; -*-

;;; Commentary:

;; - code folding
;; - hi-lock
;; - narrow

;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp-declare hs-toggle-hiding hs-show-all hs-hide-all)
(defvar hs-minor-mode)

;; -------------------------------------------------------------------
;;; Code Fold

(defvar nvp-hs--hidden nil)

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
    (hs-toggle-hiding)))

(provide 'nvp-code)
;;; nvp-code.el ends here
