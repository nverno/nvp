;;; nvp-pdf.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-27 13:22:28>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/md-tools
;; Created: 13 November 2016

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib)
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
