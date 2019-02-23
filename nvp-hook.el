;;; nvp-hook.el --- hooks -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-22 20:06:14>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 14 January 2019

;;; Commentary:
;; random hook functions
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (defvar time-stamp-pattern))

;;;###autoload
(defun nvp-hook-update-timestamp ()
  "Update buffer time stamps - `before-save-hook'."
  (require 'time-stamp)
  (let ((time-stamp-pattern 
         (or time-stamp-pattern
             (pcase major-mode
               (`org-mode "#\\+DATE: <%%>$")
               (_ "15/Last modified: <%%>$")))))
    (time-stamp)))

;;;###autoload
(defun nvp-hook-create-directory ()
  "Added to `find-file-not-found-functions'."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p 
		(format "directory `%s' does not exist! create it?" 
			parent-directory)))
      (make-directory parent-directory t))))

;; -------------------------------------------------------------------
;;; Remove lsp stuff

(eval-when-compile
  (defvar eglot--saved-bindings))
(declare-function flymake-mode "flymake")
(declare-function eglot-completion-at-point "eglot")
(declare-function eglot--eldoc-message "eglot")
(declare-function eglot-imenu "eglot")
(declare-function eglot-xref-backend "eglot")

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

(provide 'nvp-hook)
;;; nvp-hook.el ends here
