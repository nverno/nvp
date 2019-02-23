;;; nvp-apropos.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-22 17:43:49>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 30 November 2016

;;; Commentary:
;; Apropos hydra from wiki
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro)
  (require 'hydra))

;;;###autoload(autoload 'nvp-apropos-hydra/body "nvp-apropos")
(nvp-hydra-set-property 'nvp-apropos-hydra)
(defhydra nvp-apropos-hydra (:color blue)
  "Apropos"
  ("a" apropos "apropos")
  ("c" apropos-command "cmd")
  ("d" apropos-documentation "doc")
  ("e" apropos-value "val")
  ("l" apropos-library "lib")
  ("o" apropos-user-option "option")
  ("u" apropos-user-option "option")
  ("v" apropos-variable "var")
  ("i" info-apropos "info")
  ("t" xref-find-apropos "tags")
  ("z" nvp-apropos-hydra-customize/body "customize"))

(nvp-hydra-set-property 'nvp-apropos-hydra-customize)
(defhydra nvp-apropos-hydra-customize (:color blue)
  "Apropos (customize)"
  ("a" customize-apropos "apropos")
  ("f" customize-apropos-faces "faces")
  ("g" customize-apropos-groups "groups")
  ("o" customize-apropos-options "options"))

(provide 'nvp-apropos)
;;; nvp-apropos.el ends here
