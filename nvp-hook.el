;;; nvp-hook.el --- hooks -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-14 23:09:08>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 14 January 2019

;;; Commentary:
;; random hook functions
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (defvar time-stamp-pattern))
(nvp-autoload "nvp-read" nvp-read-obarray-regex nvp-read-elisp-function)

;;;###autoload
(defun nvp-hook-update-timestamp ()
  "Update buffer time stamps - `before-save-hook'."
  (require 'time-stamp)
  (let ((time-stamp-pattern 
         (or time-stamp-pattern
             (pcase major-mode
               (`org-mode "#\\+DATE: <%%>$")
               (`sh-mode "10/scriptversion=%:y-%02m-%02d.%02H$")
               (_ "15/Last modified: <%%>$")))))
    (time-stamp)))

;;;###autoload
(defun nvp-hook-add-or-remove (func hook-var hook-fn &optional append local)
  "Call FUNC to add or remove HOOK-FN from HOOK-VAR, locally when called \
interactively."
  (interactive
   (let* ((func (if current-prefix-arg 'remove-hook 'add-hook))
          (hook (intern (nvp-read-obarray-regex
                         (format "Hook variable to %s (before-save-hook): "
                                 (if (eq func 'remove-hook) "remove from" "add to"))
                         "-hook$" "before-save-hook"))))
     (list func hook
           (if (eq func 'remove-hook)
               (intern (completing-read
                        (format "Function to remove from %s: " hook)
                        (remq t (symbol-value hook))))
             (nvp-read-elisp-function (format "Function to add to %s: " hook)))
           nil t)))
  (if (eq func 'remove-hook)
      (remove-hook hook-var hook-fn local)
   (funcall func hook-var hook-fn append local)))

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
