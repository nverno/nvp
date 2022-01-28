;;; nvp-disassemble.el --- disassembly -*- lexical-binding: t; -*-

;;; Commentary:
;; FIXME: all of it
;; Mode local functions
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp:decls)

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
    (nvp:with-toggled-tip doc)))

(provide 'nvp-disassemble)
;;; nvp-disassemble.el ends here
