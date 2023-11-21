;;; nvp-llvm.el --- LLVM IR -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp:decls :p (llvm c))

;;; Help

;; TODO: local lookup -- index sphinx docs?
(defun nvp-llvm-help-at-point ()
  (interactive)
  (call-interactively #'llvm-mode-lookup-instruction-online))

;;; C-related

;; Add a cc-mode style for editing LLVM C and C++ code
(with-eval-after-load 'llvm-mode
  (c-add-style "llvm.org"
               '("gnu"
	         (fill-column         . 80)
	         (c++-indent-level    . 2)
	         (c-basic-offset      . 2)
	         (indent-tabs-mode    . nil)
	         (c-offsets-alist
                  . 
                  ((arglist-intro     . ++)
		   (innamespace       . 0)
		   (member-init-intro . ++)
		   (statement-cont    . nvp-llvm-lineup-statement)))))

  ;; From: https://github.com/llvm-mirror/llvm/utils/emacs/emacs.el
  (defun nvp-llvm-lineup-statement (langelem)
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
    (when (string-match "llvm" buffer-file-name)
      (c-set-style "llvm.org"))))

(provide 'nvp-llvm)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-llvm.el ends here
