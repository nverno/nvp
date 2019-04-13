;;; nvp-lsp.el --- unused -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(require 'nvp)
(eval-when-compile
  (defvar eglot--saved-bindings))
(declare-function flymake-mode "flymake")
(nvp-decl :pre "eglot" completion-at-point -eldoc-message imenu xref-backend)

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
