;;; nvp-rust-ts.el --- Rust tree-sitter -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'rust-ts-mode nil t)
(nvp:decls)

(defvar nvp-rust--ts-fonts
  (treesit-font-lock-rules
   :language 'rust
   :feature 'macro
   `(;; XXX(4/20/24): rust-ts-mode gives builtin face
     (macro_invocation
      ["!"] @font-lock-preprocessor-face)
     (macro_invocation
      macro: ((identifier) @font-lock-preprocessor-face
              (:match ,(rx-to-string
                        `(seq bol
                              (or ,@rust-ts-mode--builtin-macros)
                              eol))
                      @font-lock-preprocessor-face)))
     (token_tree ["!"] @font-lock-preprocessor-face)
     (token_tree
      ((identifier) @font-lock-preprocessor-face
       (:match ,(rx-to-string
                 `(seq bol
                       (or ,@rust-ts-mode--builtin-macros)
                       eol))
               @font-lock-preprocessor-face))))))

(nvp:treesit-add-rules rust-ts-mode
  :mode-fonts rust-ts-mode--font-lock-settings
  :new-fonts nvp-rust--ts-fonts)

(provide 'nvp-rust-ts)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-rust-ts.el ends here
