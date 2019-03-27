;;; nvp-disassemble.el --- disassembly -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-27 15:41:33>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created:  8 February 2019

;;; Commentary:
;; FIXME: all of it
;; Mode local functions
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'nvp-macro)
  (require 'nvp-doc))
(require 'nvp)

;; -------------------------------------------------------------------
;;; Disassemble code
;; - lisp, c, java

;;;###autoload
(defun nvp-disassemble (popup)
  "Call local function `nvp-disassemble-function'."
  (interactive "P")
  (if popup
      (nvp-disassemble-doc)
   (call-interactively nvp-disassemble-function)))

(cl-defgeneric nvp-disassemble-doc ()
  "Return docstring with disassembly."
  (user-error "`nvp-disassemble-doc' not implemented for %S" major-mode))

(cl-defmethod nvp-disassemble-doc
  (&context ((not (null (derived-mode-p 'emacs-lisp-mode 'inferior-emacs-lisp-mode))) (eql t)))
  (message "BLAH"))

(cl-defmethod nvp-disassemble-doc
  (&context (major-mode comint-mode)) ()
  (user-error "TODO"))

(defun nvp-disassemble-popup ()
  (interactive)
  (let ((doc (nvp-disassemble-doc)))
    (nvp-with-toggled-tip doc)))

(provide 'nvp-disassemble)
;;; nvp-disassemble.el ends here
