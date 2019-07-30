;;; nvp-m4.el --- m4 -*- lexical-binding: t; -*-
;;; Commentary:

;; m4 add-ons to autoconf
;;
;; TODO:
;; - add string / sh font-locking? need to understand syntax better

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'lisp-mode)
  (require 'autoconf))
(require 'autoconf)
(require 'm4-mode)
(require 'nvp-autoconf)

(defvar nvp-m4-def-regex)
(defvar nvp-m4-imenu-expression)

;; -------------------------------------------------------------------
;;; Font-locking
;; add autoconf font-locking and imenu

(eval-when-compile
  (defun nvp-m4--ac-regexp (&rest names)
    (concat "\\_<\\(A[UC]_" (regexp-opt names) "\\)\\_>(\\[\\([^\]]+\\)")))

(let-when-compile
    ((defs '("DEFUN" "DEFUN_ONCE" "ALIAS")))
  (let ((defs-re (eval-when-compile (apply #'nvp-m4--ac-regexp defs))))
    ;; font-lock functions and defuns
    (defvar nvp-m4-def-regex defs-re)
    (defvar nvp-m4-imenu-expression `(,@autoconf-imenu-generic-expression
                                      ,(list nil defs-re 1)))))

(font-lock-add-keywords
 'm4-mode
 `((,nvp-m4-def-regex
    (1 font-lock-keyword-face) (2 font-lock-function-name-face prepend))
   ,@autoconf-font-lock-keywords))

(provide 'nvp-m4)
;;; nvp-m4.el ends here
