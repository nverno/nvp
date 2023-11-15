;;; nvp-lua-ts.el --- lua tree-sitter mods -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'lua-ts-mode)
(nvp:decls :p (lua))

;; TODO: remove after patch
(setq lua-ts--font-lock-settings
      (append
       (treesit-font-lock-rules
        :language 'lua
        :feature 'string
        '((string) @font-lock-string-face)
        :language 'lua
        :feature 'escape
        :override t
        '((escape_sequence) @font-lock-escape-face))
       (seq-filter (lambda (e)
                     (not (memq (nth 2 e) '(escape string))))
                   lua-ts--font-lock-settings)))

;; non-nil to align arguments with parent
(defvar lua-ts-align-arguments nil)

;; Modify to not indent arguments weird
(setq lua-ts--simple-indent-rules
      `((lua
         ((or (node-is "comment")
              (parent-is "comment_content")
              (parent-is "string_content")
              (node-is "]]"))
          no-indent 0)
         ((and (n-p-gp "field" "table_constructor" "arguments")
               lua-ts--multi-arg-function-call-matcher)
          parent lua-ts-indent-offset)
         ((and (n-p-gp "}" "table_constructor" "arguments")
               lua-ts--multi-arg-function-call-matcher)
          parent 0)
         ((or (node-is "do")
              (node-is "then")
              (node-is "elseif_statement")
              (node-is "else_statement")
              (node-is "until")
              (node-is ")")
              (node-is "}"))
          standalone-parent 0)
         ;; Added
         ,@(if lua-ts-align-arguments
               '(((or (and (parent-is "arguments") lua-ts--first-child-matcher)
                      (and (parent-is "parameters") lua-ts--first-child-matcher)
                      (and (parent-is "table_constructor") lua-ts--first-child-matcher))
                  standalone-parent lua-ts-indent-offset)
                 ((or (parent-is "arguments")
                      (parent-is "parameters")
                      (parent-is "table_constructor"))
                  (nth-sibling 1) 0))
             `(((parent-is ,(rx (or "arguments" "parameters" "table_constructor")))
                standalone-parent lua-ts-indent-offset)))
         ((and (n-p-gp "block" "function_definition" "parenthesized_expression")
               lua-ts--nested-function-block-matcher
               lua-ts--nested-function-block-include-matcher)
          parent lua-ts-indent-offset)
         ((and (n-p-gp "block" "function_definition" "arguments")
               lua-ts--nested-function-argument-matcher)
          parent lua-ts-indent-offset)
         ((match "function_definition" "parenthesized_expression")
          standalone-parent lua-ts-indent-offset)
         ((node-is "block") standalone-parent lua-ts-indent-offset)
         ((parent-is "block") parent 0)
         ((and (node-is "end") lua-ts--end-line-matcher)
          standalone-parent lua-ts--end-indent-offset)
         ((match "end" "function_declaration") parent 0)
         ((and (n-p-gp "end" "function_definition" "parenthesized_expression")
               lua-ts--nested-function-end-argument-matcher)
          parent 0)
         ((and (n-p-gp "end" "function_definition" "parenthesized_expression")
               lua-ts--nested-function-block-matcher
               lua-ts--nested-function-end-matcher
               lua-ts--nested-function-last-function-matcher)
          parent 0)
         ((n-p-gp "end" "function_definition" "arguments") parent 0)
         ((or (match "end" "function_definition")
              (node-is "end"))
          standalone-parent 0)
         ((or (parent-is "function_declaration")
              (parent-is "function_definition")
              (parent-is "do_statement")
              (parent-is "for_statement")
              (parent-is "repeat_statement")
              (parent-is "while_statement")
              (parent-is "if_statement")
              (parent-is "else_statement")
              (parent-is "elseif_statement"))
          standalone-parent lua-ts-indent-offset)
         ((parent-is "chunk") column-0 0)
         ((parent-is "ERROR") no-indent 0))))

(provide 'nvp-lua-ts)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-lua-ts.el ends here
