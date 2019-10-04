;;; nvp-llvm.el --- LLVM IR -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp-decls)

;;; Help

(defun nvp-llvm-lookup-online (instr)
  "Lookup help for INSTR, default to thing at point, in online manual.
With prefix, query for INSTR."
  (interactive
   (list
    (or (and (not current-prefix-arg) (thing-at-point 'symbol))
        (read-from-minibuffer "Lookup help for: "))))
  (browse-url
   (format "https://llvm.org/docs/LangRef.html#%s-instruction" instr)))

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
