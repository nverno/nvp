;;; nvp-fish.el --- fish shell helpers -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-03-09 03:09:38>
;; Created:  4 November 2016

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (defvar company-keywords-alist)
  (defvar fish-builtins)
  (defvar fish-keywords))
(require 'nvp-shell)

;; add fish keywords / builtins for company completion
(defun nvp-fish-company-keywords ()
  (when (bound-and-true-p company-keywords-alist)
    (unless (assq 'fish-mode company-keywords-alist)
      (setq company-keywords-alist
            (cons
             (cons 'fish-mode (append fish-builtins fish-keywords))
             company-keywords-alist)))))

(with-eval-after-load 'company
  (nvp-fish-company-keywords))

(provide 'nvp-fish)
;;; nvp-fish.el ends here
