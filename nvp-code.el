;;; nvp-code.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-27 02:34:02>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-02-07 14:13:54>
;; Created: 25 November 2016

;;; Commentary:
;; code folding
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(nvp-declare "hideshow" hs-toggle-hiding hs-show-all hs-hide-all)
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
  (nvp-bind-keys nvp-narrow/fold-keymap
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
