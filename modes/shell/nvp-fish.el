;;; nvp-fish.el --- fish shell helpers -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-03-14 19:43:16>
;; Created:  4 November 2016

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'cl-lib))
(require 'company)
(require 'fish-mode)

;; default erases file if fish isn't installed!!
(defsubst nvp-fish-ensure-indent ()
  (executable-find "fish_indent"))

(defun nvp-fish-indent ()
  "Wrapper for fish_indent."
  (interactive)
  (and (nvp-fish-ensure-indent) (fish_indent)))

(when (executable-find "fish_indent")
  (defun nvp-fish-before-save ()
    (add-hook 'before-save-hook #'fish_indent-before-save nil t))
  (add-hook 'fish-mode-hook #'nvp-fish-before-save))

;; -------------------------------------------------------------------
;;; Completion - basic

;; add fish keywords / builtins for company completion
;; (defun company-fish)
(defun nvp-fish-add-company-keywords ()
  (when (bound-and-true-p company-keywords-alist)
    (unless (assq 'fish-mode company-keywords-alist)
      (dolist (bi fish-builtins)
        (add-text-properties 0 1 (list 'annot "<builtin>") bi))
      (dolist (kw fish-keywords)
        (add-text-properties 0 1 (list 'annot "<keyword>") kw))
      (setq company-keywords-alist
            (cons
             (cons 'fish-mode (append fish-builtins fish-keywords))
             company-keywords-alist)))))

(with-eval-after-load 'company
  (nvp-fish-add-company-keywords))

(provide 'nvp-fish)
;;; nvp-fish.el ends here
