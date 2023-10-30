;;; nvp-vhdl.el --- vhdl -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'vhdl-ts-mode nil t)
(nvp:decls)

(defface nvp-vhdl-package-face
  '((t ( :inherit font-lock-function-name-face
         :underline (:color "#9f6f6f" :style line :position t))))
  "Package face.")

(defvar nvp-vhdl-ts-font-lock
  (when (treesit-available-p)
    (treesit-font-lock-rules
     :language 'vhdl
     :feature 'number
     '([(integer_decimal)] @font-lock-number-face)
     :language 'vhdl
     :feature 'operator
     '([":=" "=>"] @font-lock-operator-face)
     :language 'vhdl
     :feature 'delimiter
     '([";" "," ":"] @font-lock-delimiter-face)
     :language 'vhdl
     :feature 'nvp
     '((constant_interface_declaration
        (identifier_list (identifier) @font-lock-variable-name-face))
       (package_instantiation_declaration
        name: (identifier) @font-lock-variable-name-face)
       (package_declaration
        name: (identifier) @nvp-vhdl-package-face
        at_end: (simple_name) @font-lock-function-name-face)
       (entity_declaration
        name: (identifier) @nvp-vhdl-package-face)
       (architecture_body
        name: (identifier) @nvp-vhdl-package-face)))))

(with-eval-after-load 'vhdl-ts-mode
  (setq vhdl-ts--treesit-settings
        (seq-uniq
         (append
          nvp-vhdl-ts-font-lock
          (seq-remove (lambda (el)
                        (memq (caddr el) (mapcar #'caddr nvp-vhdl-ts-font-lock)))
                      vhdl-ts--treesit-settings)))))

(provide 'nvp-vhdl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-vhdl.el ends here
