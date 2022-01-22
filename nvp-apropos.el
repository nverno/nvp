;;; nvp-apropos.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))

;; Apropos hydra from github wiki
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
;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not docstrings)
;; End:
;;; nvp-apropos.el ends here
