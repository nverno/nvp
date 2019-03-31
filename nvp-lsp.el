;;; nvp-lsp.el --- unused -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-31 04:34:25>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 31 March 2019

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(require 'nvp)
(eval-when-compile
  (defvar eglot--saved-bindings))
(declare-function flymake-mode "flymake")
(nvp-decl "eglot" eglot-completion-at-point eglot--eldoc-message eglot-imenu
  eglot-xref-backend)

;;;###autoload
(cl-defun nvp-hook-eglot-shutup (&key (completion t) (eldoc t) (flymake t) (imenu t)
                                      xref)
  "Remove eglot hooks/advices that clobber stuff.
By default remove all but XREF."
  (and completion
       (remove-hook 'completion-at-point-functions 'eglot-completion-at-point t))
  (and flymake (flymake-mode -1))
  (and xref (remove-hook 'xref-backend-functions 'eglot-xref-backend t))
  (when eldoc
    (remove-function (local 'eldoc-message-function) #'eglot--eldoc-message)
    (and (bound-and-true-p eglot--saved-bindings)
         (eval
           `(nvp-eldoc-function
             ,(cdr (assoc 'eldoc-documentation-function eglot--saved-bindings))))))
  (and imenu (remove-function (local 'imenu-create-index-function) #'eglot-imenu)))


(provide 'nvp-lsp)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-lsp.el ends here
