;;; nvp-ielm.el --- ielm/lisp-interaction -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; FIXME:
;; - multi-line ielm history ==> how to change and use `comint-input-ring-separator'
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'lisp-mode)
(require 'ielm)
(require 'nvp-comint)

(defvar nvp-ielm-hippie-expanders
  '(nvp-he-try-expand-history
    nvp-try-expand-flex
    nvp-try-expand-dabbrev-closest-first
    try-expand-dabbrev-all-buffers
    try-expand-dabbrev-from-kill
    try-complete-lisp-symbol
    try-complete-lisp-symbol-partially))

;; insert nl and indent - ielm-return always wants to eval when smartparens
;; close sexps
(defun nvp-ielm-nl (&optional arg)
  "Insert nl and indent unless point is at end-of-line."
  (interactive "P")
  (if (eolp) (ielm-return arg)
   (let (ielm-dynamic-return)
     (newline-and-indent))))

;; synchronize with default switching function and update default-directory
(with-no-warnings
  (define-advice ielm-change-working-buffer (:after (&rest _args) "update-src")
    (with-current-buffer (process-buffer (ielm-process))
      (process-put (ielm-process) :src-buffer ielm-working-buffer)
      (setq default-directory
            (buffer-local-value 'default-directory ielm-working-buffer)))))


;;; Fonts

(defconst nvp-ielm-font-lock-keywords
  `(,@(mapcar #'nvp-comint-font-lock-keywords lisp-el-font-lock-keywords-2)
    ,@(mapcar #'nvp-comint-font-lock-keywords lisp-cl-font-lock-keywords-2)))

(nvp-font-lock-add-defaults 'inferior-emacs-lisp-mode
  ((nvp:re-opt '("IELM error" "Eval error" "Read error")) .
   (0 font-lock-warning-face prepend))
  (:splice
   (mapcar #'nvp-comint-font-lock-keywords lisp-el-font-lock-keywords-2))
  (:splice
   (mapcar #'nvp-comint-font-lock-keywords lisp-cl-font-lock-keywords-2)))

;; (font-lock-add-keywords 'inferior-emacs-lisp-mode nvp-ielm-font-lock-keywords)

(provide 'nvp-ielm)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-ielm.el ends here
