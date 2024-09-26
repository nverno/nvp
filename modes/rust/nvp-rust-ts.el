;;; nvp-rust-ts.el --- Rust tree-sitter -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'rust-ts-mode nil t)
(nvp:decls)

(defvar nvp-rust--ts-fonts
  (cons
   (treesit-font-lock-rules
    :language 'rust
    :feature 'macro
    `([(attribute_item) (inner_attribute_item)] @font-lock-preprocessor-face

      ;; These are operators in types
      (type_parameters ["<" ">"] @font-lock-bracket-face)
      (type_arguments ["<" ">"] @font-lock-bracket-face)

      (constrained_type_parameter
       left: (type_identifier) @font-lock-variable-name-face)

      ;; lifetimes
      (lifetime "'" @font-lock-operator-face)
      (lifetime ((identifier) @font-lock-keyword-face
                 (:match ,(rx bol "static" eol) @font-lock-keyword-face)))
      (lifetime (identifier) @font-lock-variable-name-face)

      (macro_invocation
       (scoped_identifier
        name: (identifier) @font-lock-preprocessor-face)
       :anchor "!" @font-lock-preprocessor-face)

      ;; XXX(4/20/24): rust-ts-mode gives builtin face
      (macro_invocation ["!"] @font-lock-preprocessor-face)
      (macro_invocation
       macro: ((identifier) @font-lock-preprocessor-face
               (:match ,(rx-to-string
                         `(seq bol
                               (or ,@rust-ts-mode--builtin-macros)
                               eol))
                       @font-lock-preprocessor-face)))
      ;; (token_tree ["!"] @font-lock-preprocessor-face)
      ;; (token_tree
      ;;  ((identifier) @font-lock-preprocessor-face
      ;;   (:match ,(rx-to-string
      ;;             `(seq bol
      ;;                   (or ,@rust-ts-mode--builtin-macros)
      ;;                   eol))
      ;;           @font-lock-preprocessor-face)
      ;;   "!" @font-lock-preprocessor-face))
      ;; Nested macros
      (token_tree (identifier) @font-lock-preprocessor-face
                  :anchor "!" @font-lock-preprocessor-face)
      (token_repetition (identifier) @font-lock-preprocessor-face
                        :anchor "!" @font-lock-preprocessor-face))

    :language 'rust
    :feature 'nvp
    '((field_expression
       value: (identifier) @font-lock-receiver-face)
      ;; XXX(08/14/24): patch for missing labels
      (label
       "'" @font-lock-operator-face
       (identifier) @font-lock-constant-face)
      ;; Type definitions
      (struct_item name: (type_identifier) @font-lock-type-def-face)))
   
   ;; FIXME(4/26/24): messes up attributes
   (treesit-font-lock-rules
    :language 'rust
    :feature 'escape-sequence
    :override t
    '((string_literal) @font-lock-string-face
      (string_literal (escape_sequence) @font-lock-escape-face)))))


(nvp:treesit-add-rules rust-ts-mode
  :mode-fonts rust-ts-mode--font-lock-settings
  :new-fonts (car nvp-rust--ts-fonts)
  :post-fonts (cdr nvp-rust--ts-fonts))

(provide 'nvp-rust-ts)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-rust-ts.el ends here
