;;; nvp-fish.el --- fish shell helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decl-prefix fish)
(require 'company)
(require 'fish-mode nil t)

;; default erases file if fish isn't installed!!
(defsubst nvp-fish-ensure-indent ()
  (executable-find "fish_indent"))

(defun nvp-fish-indent ()
  "Wrapper for fish_indent."
  (interactive)
  (and (nvp-fish-ensure-indent) (fish_indent)))

(defun nvp-fish-before-save ()
  (when (executable-find "fish_indent")
    (add-hook 'before-save-hook #'fish_indent-before-save nil t)))
(when (executable-find "fish_indent")
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
