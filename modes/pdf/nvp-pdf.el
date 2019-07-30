;;; nvp-pdf.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (defvar pdf-info-epdfinfo-program))
(require 'pdf-tools)

;; ------------------------------------------------------------
;;; Setup

(defvar pdf-tools-auto-mode-alist-entry)
(defun nvp-pdf-setup ()
  (unless (file-exists-p pdf-info-epdfinfo-program)
    (pdf-tools-install t t t)
    ;; (call-interactively 'nvp-pdf-install-epdfinfo))
    ;; add enty that `pdf-tools-install' would add
    (add-to-list 'auto-mode-alist pdf-tools-auto-mode-alist-entry)))

(provide 'nvp-pdf)
;;; nvp-pdf.el ends here
