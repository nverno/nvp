;;; nvp-go-ts.el --- Go tree-sitter mods -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'go-ts-mode nil t)
(require 'nvp)                          ; `nvp-receiver-face'
(nvp:decls)


(cl-defmethod nvp-go-in-literal-p
  (&context (major-mode go-ts-mode) &optional marker &rest _)
  (string-match-p
   "literal"                            ; literal_value / composite_literal
   (treesit-node-type
    (treesit-node-parent
     (treesit-node-at
      (if marker (marker-position marker) (point)))))))

;;; Indentation:
;; Modify to indent /* ... */
(require 'c-ts-common)                  ; comment indentation + filling
(defvar nvp-go-ts--indent-rules
  `(((and (parent-is "comment") c-ts-common-looking-at-star)
     c-ts-common-comment-start-after-first-star -1)
    ((parent-is "comment") prev-adaptive-prefix 0))
  "Additional indent rules to handle comments.")

;;; Font-locking
(defvar nvp-go-ts-font-lock-settings
  (when (require 'go-ts-mode nil t)
    (cl-pushnew ":" go-ts-mode--operators :test #'equal)
    (treesit-font-lock-rules
     :language 'go
     :feature 'operator
     `([,@go-ts-mode--operators] @font-lock-operator-face)
     :language 'go
     :feature 'namespace
     '((selector_expression
        operand: (identifier) @nvp-receiver-face))
     :language 'go
     :feature 'nvp
     `(;; Functions declared with var specs
       ;; Note: patch not wanted
       (var_spec name: (identifier) @font-lock-function-name-face
                 ("," name: (identifier) @font-lock-function-name-face)*
                 type: (function_type))
       ;; XXX(09/05/24): remove after patch
       (type_parameter_declaration
        name: (identifier) @font-lock-type-face)))))

(nvp:treesit-add-rules go-ts-mode
  :new-fonts nvp-go-ts-font-lock-settings
  :mode-fonts go-ts-mode--font-lock-settings
  :new-indents nvp-go-ts--indent-rules
  :mode-indents go-ts-mode--indent-rules)

(provide 'nvp-go-ts)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-go-ts.el ends here
