;;; nvp-lua-ts.el --- lua tree-sitter mods -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'lua-ts-mode nil t)
(require 'nvp)                          ; faces
(nvp:decls :p (lua) :v (lua-ts--builtins lua-ts-mode-hook))


(defcustom lua-ts-align-arguments nil
  "Non-nil to align arguments with parent"
  :type 'boolean
  :safe 'booleanp
  :group 'lua)


;;; Font-locking

(setq lua-ts--builtins
      (seq-uniq
       (append '("jit"
                 "__add" "__band" "__bnot" "__bor" "__bxor" "__call" "__concat"
                 "__div" "__eq" "__gc" "__idiv" "__index" "__le" "__len" "__lt"
                 "__metatable" "__mod" "__mul" "__name" "__newindex" "__pairs"
                 "__pow" "__shl" "__shr" "__sub" "__tostring" "__unm")
               lua-ts--builtins)
       #'string=))

(defvar nvp-lua-ts--identifier-query
  (when (treesit-available-p)
    (treesit-query-compile 'lua '((identifier) @id))))

(defun nvp-lua-ts--fontify-function-decl (node override start end &rest _)
  (let ((type (treesit-node-type node))
        face table)
    (pcase type
      ("identifier" nil)
      ("method_index_expression"
       (setq face 'nvp-namespace-use-face
             table (treesit-node-child-by-field-name node "table")
             node (treesit-node-child-by-field-name node "method")))
      ("dot_index_expression"
       (setq face 'nvp-namespace-face
             table (treesit-node-child-by-field-name node "table")
             node (treesit-node-child-by-field-name node "field"))))
    (treesit-fontify-with-override
     (treesit-node-start node) (treesit-node-end node)
     'font-lock-function-name-face override start end)
    (when table
      (dolist (node (treesit-query-capture
                     table nvp-lua-ts--identifier-query start end t))
        (treesit-fontify-with-override
         (treesit-node-start node) (treesit-node-end node)
         face override start end)))))

(defun nvp-lua-ts--fontify-table (node override start end &rest _)
  (let ((face 'nvp-receiver-face))
    (when-let* ((node (pcase (treesit-node-type node)
                        ("method_index_expression"
                         (prog1 nil
                           (nvp-lua-ts--fontify-table
                            (treesit-node-child-by-field-name node "table")
                            override start end)))
                        ("dot_index_expression"
                         (setq face 'nvp-namespace-use-face)
                         (treesit-node-child-by-field-name node "table"))
                        ("identifier" node))))
      (dolist (node (treesit-query-capture
                     node nvp-lua-ts--identifier-query start end t))
        (treesit-fontify-with-override
         (treesit-node-start node) (treesit-node-end node)
         face override start end)))))

(defun nvp-lua-ts--fontify-doc-comment (node override start end &rest _)
  (let ((node-start (treesit-node-start node)))
    (save-excursion
      (goto-char node-start)
      (skip-chars-forward " \t-" (line-end-position))
      (when (and (< (point) end)
                 (looking-at "[ \t]*\\(@[[:alpha:]]*\\)"))
        (treesit-fontify-with-override
         (match-beginning 1) (match-end 1) 'font-lock-doc-markup-face
         override start end)))))

(defvar nvp-lua-ts--font-lock-rules
  (when (treesit-available-p)
    (cons
     ;; Before other rules
     (treesit-font-lock-rules
      :language 'lua
      :feature 'definition
      '((function_declaration
         name: (identifier) @font-lock-function-name-face)
        (function_declaration
         name: [(dot_index_expression) (method_index_expression)]
         @nvp-lua-ts--fontify-function-decl)
        (field name: (identifier) @font-lock-function-name-face
               value: (function_definition))
        (parameters name: (identifier) @font-lock-variable-name-face))

      :language 'lua
      :feature 'builtin
      `(((identifier) @font-lock-keyword-face
         (:match ,(rx bos "self" eos) @font-lock-keyword-face))
        ((identifier) @font-lock-builtin-face
         (:match ,(rx-to-string `(seq bos (or ,@lua-ts--builtins) eos))
                 @font-lock-builtin-face))))

     ;; After other rules
     (treesit-font-lock-rules
      :language 'lua
      :feature 'comment
      `(((comment) @font-lock-doc-face
         (:match ,(rx bos "--" (or "-" (seq (* (any "-" " " "\t")) "@")))
                 @font-lock-doc-face))
        (comment) @lua-ts--comment-font-lock
        (hash_bang_line) @nvp-treesit-fontify-hash-bang)

      :language 'lua
      :feature 'doc
      :override t
      '(((comment) @nvp-lua-ts--fontify-doc-comment
         (:match "\\`--[- \t]*@" @nvp-lua-ts--fontify-doc-comment)))

      :language 'lua
      :feature 'namespace
      '((function_call
         name: [(method_index_expression) (dot_index_expression)]
         @nvp-lua-ts--fontify-table)
        (dot_index_expression
         table: (_) @nvp-lua-ts--fontify-table))

      :language 'lua
      :feature 'variable
      '((identifier) @font-lock-variable-use-face)))))


;;; Indentation

(defvar nvp-lua-ts--indent-rules
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
     ((parent-is "chunk") column-0 0))))


;;; Add luadoc support

;; (defvar lua-ts--treesit-luadoc-beginning-regexp "\\`--[- \t]*@"
;;   "Regular expression matching the beginning of a luadoc block comment.")

;; (defun lua-ts-language-at-point (point)
;;   "Return the language at POINT."
;;   (let ((node (treesit-node-at point 'lua)))
;;     (if (and (treesit-ready-p 'luadoc)
;;              (equal (treesit-node-type node) "comment")
;;              (string-match-p
;;               lua-ts--treesit-luadoc-beginning-regexp
;;               (treesit-node-text node)))
;;         'luadoc
;;       'lua)))

;; (defun lua-ts--enable-luadoc ()
;;   (when (treesit-ready-p 'luadoc t)
;;     (setq-local treesit-range-settings
;;                 (treesit-range-rules
;;                  :embed 'luadoc
;;                  :host 'lua
;;                  `(((comment) @capture
;;                     (:match ,lua-ts--treesit-luadoc-beginning-regexp
;;                             @capture)))))))
;; (add-hook 'lua-ts-mode-hook #'lua-ts--enable-luadoc)

(nvp:treesit-add-rules lua-ts-mode
  :mode-fonts lua-ts--font-lock-settings
  :new-fonts (car nvp-lua-ts--font-lock-rules)
  :post-fonts (cdr nvp-lua-ts--font-lock-rules)
  (setq lua-ts--simple-indent-rules
        nvp-lua-ts--indent-rules))


(provide 'nvp-lua-ts)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-lua-ts.el ends here
