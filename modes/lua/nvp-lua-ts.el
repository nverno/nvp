;;; nvp-lua-ts.el --- lua tree-sitter mods -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'treesit nil t)
(nvp:decls :p (lua) :v (lua-ts--builtins lua-ts-mode-hook))

(defvar lua-ts-align-arguments nil
  "Non-nil to align arguments with parent")

(defvar lua-ts--simple-indent-rules-orig)
(defvar lua-ts--font-lock-settings-orig)
(defvar nvp-lua-ts-font-lock-before)
(defvar nvp-lua-ts-font-lock-after)

;; (defun lua-ts-mode--assignment-identifier (node)
;;   (pcase (treesit-node-type node)
;;     ("identifier" node)
;;     ((or "bracket_index_expression" "dot_index_expression")
;;      (lua-ts-mode--assignment-identifier
;;       (treesit-node-child-by-field-name node "table")))
;;     (_ (error "shouldnt happen: %S" node))))

;; (defun lua-ts-mode--fontify-assignment-lhs (node override start end &rest _)
;;   "Fontify the lhs NODE of an assignment_exp.
;; For OVERRIDE, START, END, see `treesit-font-lock-rules'."
;;   (let ((identifier (lua-ts-mode--assignment-identifier node)))
;;     (when identifier
;;       (treesit-fontify-with-override
;;        (treesit-node-start identifier) (treesit-node-end identifier)
;;        'font-lock-variable-name-face
;;        override start end))))

(defvar lua-ts-mode--identifier-query
  (when (treesit-available-p)
    (treesit-query-compile 'lua '((identifier) @id))))

(defun lua-ts-mode--fontify-table (node override start end &rest _)
  (when-let* ((face 'font-lock-type-face)
              (node (pcase (treesit-node-type node)
                      ("method_index_expression"
                       (treesit-node-child-by-field-name node "table"))
                      ("dot_index_expression"
                       (setq face 'font-lock-constant-face)
                       (treesit-node-child-by-field-name node "table"))
                      ("identifier" nil))))
    (dolist (node (treesit-query-capture
                   node lua-ts-mode--identifier-query nil nil t))
      (treesit-fontify-with-override
       (treesit-node-start node) (treesit-node-end node)
       face override start end))))

(with-eval-after-load 'lua-ts-mode
  (setq lua-ts--font-lock-settings-orig lua-ts--font-lock-settings
        lua-ts--simple-indent-rules-orig lua-ts--simple-indent-rules
        lua-ts--builtins (--filter (not (string= it "self")) lua-ts--builtins))

  (setq nvp-lua-ts-font-lock-before
        (treesit-font-lock-rules
         :language 'lua
         :feature 'definition
         '((function_declaration
            name: (identifier) @font-lock-function-name-face)
           (function_declaration
            name: (dot_index_expression (identifier) @font-lock-function-name-face))
           (function_declaration
            name: (method_index_expression
                   table: (_) @lua-ts-mode--fontify-table
                   method: (identifier) @font-lock-function-name-face))
           (function_declaration
            (method_index_expression
             (dot_index_expression
              table: (_) @lua-ts-mode--fontify-table
              field: (identifier) @font-lock-property-name-face)))
           (assignment_statement
            (variable_list name: [(identifier)]) @font-lock-function-name-face
            (expression_list value: (function_definition)))
           (field
            name: (identifier) @font-lock-function-name-face
            value: (function_definition))
           (parameters
            name: (identifier) @font-lock-variable-name-face))

         :language 'lua
         :feature 'builtin
         `(((identifier) @font-lock-keyword-face
            (:match ,(rx bos "self" eos) @font-lock-keyword-face))
           ((identifier) @font-lock-builtin-face
            (:match ,(rx-to-string `(seq bos (or ,@lua-ts--builtins) eos))
                    @font-lock-builtin-face)))))

  (setq nvp-lua-ts-font-lock-after
        (treesit-font-lock-rules
         :language 'lua
         :feature 'comment
         '(((comment) @font-lock-doc-face
            (:match "\\`---" @font-lock-doc-face))
           (comment) @lua-ts--comment-font-lock
           (hash_bang_line) @font-lock-comment-face)

         :language 'lua
         :feature 'namespace
         ;; :override t
         '((function_declaration
            name: [(method_index_expression) (dot_index_expression)]
            @lua-ts-mode--fontify-table)
           (function_call
            name: [(method_index_expression) (dot_index_expression)]
            @lua-ts-mode--fontify-table))

         ;; :language 'lua
         ;; :feature 'assignment
         ;; '((for_numeric_clause name: (identifier) @font-lock-variable-name-face)
         ;;   (variable_list (_) @lua-ts-mode--fontify-assignment-lhs))

         :language 'lua
         :feature 'variable
         '((function_call
            arguments: (arguments (identifier) @font-lock-variable-use-face))
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

;;; Add missing features once
(nvp:run-once lua-ts-mode (:after (&rest _))
  (dolist (v '(namespace))
    (cl-pushnew v (cadddr treesit-font-lock-feature-list)))
  (nvp-lua-ts-load-mods nil t)
  (treesit-font-lock-recompute-features))

(provide 'nvp-lua-ts)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-lua-ts.el ends here
