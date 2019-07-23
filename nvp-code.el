;;; nvp-code.el ---  -*- lexical-binding: t; -*-

;;; Commentary:

;; - code folding
;; - hi-lock

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(nvp-declare hs-toggle-hiding hs-show-all hs-hide-all)
(defvar hs-minor-mode)

;; -------------------------------------------------------------------
;;; Code Fold

(defvar nvp-hs--hidden nil)

(defun nvp-hs-toggle (&optional arg)
  (interactive "P")
  (unless hs-minor-mode
    (hs-minor-mode))
  (if arg
      (if (setq nvp-hs--hidden (not nvp-hs--hidden))
          (hs-show-all)
        (hs-hide-all))
    (hs-toggle-hiding)))

;;;###autoload
(defun nvp-hs-init (&optional arg)
  (interactive "P")
  (hs-minor-mode)
  (nvp-bind-keys narrow-map
    ("f"   . nil)
    ("f f" . nvp-hs-toggle)
    ("f a" . hs-hide-all)
    ("f s" . hs-show-all))
  (nvp-hs-toggle arg))

;; -------------------------------------------------------------------
;;; Hi-lock
(eval-when-compile (defvar hi-lock-file-patterns))

;;;###autoload
(defun nvp-hi-lock-forward ()
  "Jump between hi-lock matches."
  (interactive)
  (goto-char
   (apply
    #'min
    (mapcar
     (lambda (pattern)
       (save-excursion
         (re-search-forward (car pattern) nil 'noerror)
         (point)))
     hi-lock-file-patterns))))

(provide 'nvp-code)
;;; nvp-code.el ends here
