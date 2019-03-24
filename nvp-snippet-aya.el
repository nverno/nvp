;;; nvp-snippet-aya.el --- auto-snippets -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-24 04:49:59>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 12 February 2019

;;; Commentary:
;; auto yasnippets
;;; Code:
(eval-when-compile
  (defvar aya-current))
(require 'auto-yasnippet)
(nvp-declare "" nvp-jump-to-new-snippet)

(defvar nvp-aya-new-template "\
# -*- mode: snippet -*-
# name: $1
# key: ${2:${1:$(yas--key-from-desc yas-text)}}
# --
`aya-current`")

;;;###autoload(autoload 'nvp-aya-hydra/body "nvp-snippet-aya")
(nvp-hydra-set-property 'nvp-aya-hydra)
(defhydra nvp-aya-hydra (:color blue)
  ("c" aya-create "create(~)")
  ("e" aya-expand "expand")
  ("j" nvp-jump-to-aya-snippet "jump/persist")
  ("o" aya-create-one-line "one-liner($)"))

;;;###autoload
(defun nvp-jump-to-aya-snippet ()
  "Save previously created auto-yasnippet."
  (interactive
   (if (string-empty-p aya-current)
       (user-error "No current auto-snippet.")
     (list aya-current)))
  (nvp-jump-to-new-snippet major-mode nvp-mode-snippet-dir nil aya-current
                           nvp-aya-new-template))

(provide 'nvp-snippet-aya)
;;; nvp-snippet-aya.el ends here
