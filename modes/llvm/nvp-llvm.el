;;; nvp-llvm.el --- LLVM IR -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'llvm-mode)
(require 'nvp)
(nvp-decls :f (c-lineup-assignments c-set-style) :v (c-basic-offset))

;;; Help

(defun nvp-llvm-help-at-point ()
  (interactive)
  ;; TODO: local lookup -- index sphinx docs?
  (call-interactively #'nvp-llvm-lookup-online))

;;; C-related

;; From: https://github.com/llvm-mirror/llvm/utils/emacs/emacs.el
(defun llvm-lineup-statement (langelem)
  (let ((in-assign (c-lineup-assignments langelem)))
    (if (not in-assign)
        '++
      (aset in-assign 0
            (+ (aref in-assign 0)
               (* 2 c-basic-offset)))
      in-assign)))

;; Files with "llvm" in their names will automatically be set to the
;; llvm.org coding style.
;; add to `c-mode-common-hook' to apply by default
(defun nvp-llvm-c-set-style ()
  (if (string-match "llvm" buffer-file-name)
      (progn
	(c-set-style "llvm.org"))))

(provide 'nvp-llvm)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-llvm.el ends here
