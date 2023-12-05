;;; nvp-lua-ts.el --- lua tree-sitter mods -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'treesit nil t)
(nvp:decls :p (lua))

(defvar lua-ts-align-arguments nil
  "Non-nil to align arguments with parent")

(defvar lua-ts--simple-indent-rules-orig)
(defvar lua-ts--font-lock-settings-orig)
(defvar nvp-lua-ts-font-lock-before)
(defvar nvp-lua-ts-font-lock-after)

(defvar nvp-lua-ts-mode--assignment-query
  (when (treesit-available-p)
    (treesit-query-compile 'lua '((identifier) @id)))
  "Query to capture identifiers in assignment_exp.")

(defun nvp-lua-ts-mode--fontify-assignment-lhs (node override start end &rest _)
  "Fontify the lhs NODE of an assignment_exp.
For OVERRIDE, START, END, see `treesit-font-lock-rules'."
  (dolist (node (treesit-query-capture
                 node nvp-lua-ts-mode--assignment-query nil nil t))
    (treesit-fontify-with-override
     (treesit-node-start node) (treesit-node-end node)
     (pcase (treesit-node-type node)
       ("identifier" 'font-lock-variable-name-face))
     override start end)))

(with-eval-after-load 'lua-ts-mode
  (setq lua-ts--font-lock-settings-orig lua-ts--font-lock-settings
        lua-ts--simple-indent-rules-orig lua-ts--simple-indent-rules)
  
  (cl-pushnew "self" lua-ts--builtins :test #'string=)

  (setq nvp-lua-ts-font-lock-before
        (treesit-font-lock-rules
         :language 'lua
         :feature 'definition
         '((function_declaration
            name: (identifier) @font-lock-function-name-face)
           (assignment_statement
            (variable_list name: [(identifier)]) @font-lock-function-name-face
            (expression_list value: (function_definition)))
           (table_constructor
            (field
             name: (identifier) @font-lock-function-name-face
             value: (function_definition)))
           (function_declaration
            name: (dot_index_expression (identifier) @font-lock-function-name-face))
           (function_declaration
            name: (method_index_expression
                   table: (identifier) @font-lock-type-face
                   method: (identifier) @font-lock-function-name-face))
           (function_declaration
            (method_index_expression
             (dot_index_expression
              table: (identifier) @font-lock-type-face
              field: (identifier) @font-lock-property-name-face)))
           (parameters
            name: (identifier) @font-lock-variable-name-face)
           (for_numeric_clause name: (identifier) @font-lock-variable-name-face))))

  (setq nvp-lua-ts-font-lock-after
        (treesit-font-lock-rules
         :language 'lua
         :feature 'comment
         '(((comment) @font-lock-doc-face
            (:match "\\`---" @font-lock-doc-face))
           (comment) @lua-ts--comment-font-lock
           (hash_bang_line) @font-lock-comment-face)

         :language 'lua
         :feature 'assignment
         '((variable_list) @nvp-lua-ts-mode--fontify-assignment-lhs)
         ;; '((variable_list
         ;;    [(identifier)
         ;;     (bracket_index_expression)]
         ;;    @font-lock-variable-name-face)
         ;;   (variable_list
         ;;    (dot_index_expression
         ;;     table: (identifier))
         ;;    @font-lock-variable-name-face))
         :language 'lua
         :feature 'variable
         '((function_call
            arguments: (arguments (identifier) @font-lock-variable-use-face))
           (function_call
            name: (method_index_expression
                   table: (identifier) @font-lock-type-face))
           [(identifier)] @font-lock-variable-use-face))))

;;;###autoload
(defun nvp-lua-ts-load-font-lock (&optional orig)
  (interactive "P")
  (setq lua-ts--font-lock-settings
        (if orig
            lua-ts--font-lock-settings-orig
          (let ((nvp-features (--map (nth 2 it)
                                     (append nvp-lua-ts-font-lock-before
                                             nvp-lua-ts-font-lock-after))))
            (append nvp-lua-ts-font-lock-before
                    (--filter (not (memq (nth 2 it) nvp-features))
                              lua-ts--font-lock-settings-orig)
                    nvp-lua-ts-font-lock-after)))))

;;;###autoload
(defun nvp-lua-ts-load-indent (&optional orig)
  (interactive "P")
  (setq lua-ts--simple-indent-rules
        (if orig
            lua-ts--simple-indent-rules-orig
          ;; Modify to not indent arguments weird
          `((lua
             ((or (node-is "comment")
                  (parent-is "comment_content")
                  (parent-is "string_content")
                  (node-is "]]"))
              no-indent 0)
             ((and (n-p-gp "field" "table_constructor" "arguments")
                   lua-ts--multi-arg-function-call-matcher)
              parent-bol lua-ts-indent-offset)
             ((and (n-p-gp "}" "table_constructor" "arguments")
                   lua-ts--multi-arg-function-call-matcher)
              parent-bol 0)
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
                      (nth-sibling 1) 0)
                     ((and (n-p-gp "block" "function_definition" "arguments")
                           lua-ts--nested-function-argument-matcher)
                      parent lua-ts-indent-offset)
                     ((n-p-gp "end" "function_definition" "arguments") parent 0))
                 `(((parent-is ,(rx (or "arguments" "parameters" "table_constructor")))
                    standalone-parent lua-ts-indent-offset)
                   ((n-p-gp "block" "function_definition" "arguments")
                    standalone-parent lua-ts-indent-offset)
                   ((n-p-gp "end" "function_definition" "arguments") standalone-parent 0)))
             ((and (n-p-gp "block" "function_definition" "parenthesized_expression")
                   lua-ts--nested-function-block-matcher
                   lua-ts--nested-function-block-include-matcher)
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
             ((parent-is "ERROR") no-indent 0))))))

;;;###autoload
(defun nvp-lua-ts-load-mods (&optional orig interactive)
  (interactive "P")
  (nvp-lua-ts-load-font-lock orig)
  (nvp-lua-ts-load-indent orig)
  (when interactive
    (let (lua-ts-mode-hook)
      (lua-ts-mode))))

(provide 'nvp-lua-ts)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-lua-ts.el ends here
