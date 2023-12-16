;;; nvp-typescript.el --- typescript -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls :p (typescript))

(eval-and-compile
  (defvar nvp-typescript-modes
    '(typescript-mode typescript-ts-mode typescript-tsx-mode tsx-ts-mode)))

(nvp:defmethod nvp-newline-dwim-comment (syntax arg)
   :modes nvp-typescript-modes
   (nvp-newline-dwim--comment syntax arg " * "))

(with-eval-after-load 'nvp-repl
  (require 'nvp-typescript-repl))

;; -------------------------------------------------------------------
;;; `typescript-ts-mode' patches

(defvar typescript-ts-mode--operators
  '("=>" "?" "??" "..."                 ; added 12/15/23
    "=" "+=" "-=" "*=" "/=" "%=" "**=" "<<=" ">>=" ">>>=" "&=" "^="
    "|=" "&&=" "||=" "??=" "==" "!=" "===" "!==" ">" ">=" "<" "<=" "+"
    "-" "*" "/" "%" "++" "--" "**" "&" "|" "^" "~" "<<" ">>" ">>>"
    "&&" "||" "!" "?.")
  "TypeScript operators for tree-sitter font-locking.")

(require 'typescript-ts-mode)

(defvar typescript-ts-mode--assignment-lhs-query
  (when (treesit-available-p)
    (treesit-query-compile
     'typescript
     '((subscript_expression object: [(this) (identifier)] @id)
       (member_expression object: [(this) (identifier)] @id)
       (member_expression property: (property_identifier) @property))))
  "Query that captures object, index, and property identifiers.")

(defvar tsx-ts-mode--assignment-lhs-query
  (when (treesit-available-p)
    (treesit-query-compile
     'tsx
     '((subscript_expression object: [(this) (identifier)] @id)
       (member_expression object: [(this) (identifier)] @id)
       (member_expression property: (property_identifier) @property))))
  "Query that captures object, index, and property identifiers.")

(defvar-local typescript-ts-mode--assignment-query
    typescript-ts-mode--assignment-lhs-query)

(defun typescript-ts-mode--fontify-lhs (language)
  (when (eq 'tsx language)
    (setq-local typescript-ts-mode--assignment-query
                tsx-ts-mode--assignment-lhs-query))
  #'typescript-ts-mode--fontify-assignment-lhs)

(defun typescript-ts-mode--fontify-assignment-lhs (node override start end &rest _)
  "Fontify the lhs NODE of an assignment_expression.
For OVERRIDE, START, END, see `treesit-font-lock-rules'."
  ;; when INDEX > 1, apply `-*use-face' to identifiers/properties
  (cl-block nil
    (let ((index 0))
      (pcase-dolist (`(,name . ,node)
                     (treesit-query-capture
                      node typescript-ts-mode--assignment-query))
        (and (> index 1) (cl-return))
        (let ((face (pcase name
                      ('id (prog1 (if (zerop index)
                                      'font-lock-variable-name-face
                                    'font-lock-variable-use-face)
                             (cl-incf index)))
                      ('property (if (= 1 index)
                                     'font-lock-property-name-face
                                   'font-lock-property-use-face))
                      ('index 'font-lock-variable-use-face)
                      (_ nil))))
          (when (and face (not (treesit-node-match-p node "this")))
            (treesit-fontify-with-override
             (treesit-node-start node) (treesit-node-end node)
             face override start end)))))))

(defvar typescript-ts-mode-variable-builtins
  '("self" "arguments" "module" "console" "window" "document"))

(defvar typescript-ts-mode-function-builtins
  '("eval"
    "isFinite"
    "isNaN"
    "parseFloat"
    "parseInt"
    "decodeURI"
    "decodeURIComponent"
    "encodeURI"
    "encodeURIComponent"
    "require"))

(defvar typescript-ts-mode-type-builtins
  '("Object"
    "Function"
    "Boolean"
    "Symbol"
    "Number"
    "Math"
    "Date"
    "String"
    "RegExp"
    "Map"
    "Set"
    "WeakMap"
    "WeakSet"
    "Promise"
    "Array"
    "Int8Array"
    "Uint8Array"
    "Uint8ClampedArray"
    "Int16Array"
    "Uint16Array"
    "Int32Array"
    "Uint32Array"
    "Float32Array"
    "Float64Array"
    "ArrayBuffer"
    "DataView"
    "Error"
    "EvalError"
    "InternalError"
    "RangeError"
    "ReferenceError"
    "SyntaxError"
    "TypeError"
    "URIError"))

(defun nvp-typescript-ts-font-lock-rules (language)
  (treesit-font-lock-rules
   :language language
   :feature 'builtin
   `(((identifier) @_var
      (:match ,(rx-to-string
                `(seq bos (or ,@typescript-ts-mode-variable-builtins
                              ,@typescript-ts-mode-type-builtins
                              ,@typescript-ts-mode-function-builtins)
                      eos))
              @_var))
     @font-lock-builtin-face)
   
   :language language
   :feature 'property
   `((property_signature
      name: (property_identifier) @font-lock-property-name-face)
     (public_field_definition
      name: (property_identifier) @font-lock-property-name-face)
     (index_signature
      name: (identifier) @font-lock-property-name-face)
     (pair key: (property_identifier) @font-lock-property-use-face)
     ((shorthand_property_identifier) @font-lock-property-use-face))

   :language language
   :feature 'assignment
   '((assignment_expression
      left: (identifier) @font-lock-variable-name-face)
     (assignment_expression
      left: (_) @typescript-ts-mode--fontify-assignment-lhs)
     (augmented_assignment_expression
      left: (identifier) @font-lock-variable-name-face)
     (augmented_assignment_expression
      left: (_) @typescript-ts-mode--fontify-assignment-lhs)
     (update_expression
      argument: (identifier) @font-lock-variable-name-face)
     (update_expression
      argument: (_) @typescript-ts-mode--fontify-assignment-lhs))

   :language language
   :feature 'delimiter
   '((["," "." ";" ":" "?:"]) @font-lock-delimiter-face)

   :language language
   :feature 'namespace
   '(["module" "global"] @font-lock-keyword-face
     (internal_module
      name: (identifier) @font-lock-type-face)
     (internal_module
      name: (nested_identifier
             [(identifier) (property_identifier)] @font-lock-type-face))
     (module
      name: (identifier) @font-lock-function-name-face)
     (module
      name: (nested_identifier
             [(identifier) (property_identifier)] @font-lock-function-name-face)))
   
   :language language
   :feature 'variable
   '((identifier) @font-lock-variable-use-face
     (property_identifier) @font-lock-property-use-face)))

(defun nvp@typescript-ts-font-lock (orig-fn language)
  (when (eq 'tsx language)
    (setq-local typescript-ts-mode--assignment-query
                tsx-ts-mode--assignment-lhs-query))
  (let* ((new-rules (nvp-typescript-ts-font-lock-rules language))
         (features (--map (nth 2 it) new-rules))
         (rules (--filter (not (memq (nth 2 it) features))
                          (funcall orig-fn language))))
    (cons (car new-rules)               ; builtins first
          (append rules (cdr new-rules)))))

(advice-add 'typescript-ts-mode--font-lock-settings
            :around #'nvp@typescript-ts-font-lock)

;;; Add missing features once
(nvp:run-once typescript-ts-mode (:after (&rest _))
  (dolist (v '(variable builtin namespace assignment))
    (cl-pushnew v (cadddr treesit-font-lock-feature-list)))
  (treesit-font-lock-recompute-features))

(nvp:run-once tsx-ts-mode (:after (&rest _))
  (dolist (v '(operator variable builtin namespace assignment))
    (cl-pushnew v (cadddr treesit-font-lock-feature-list)))
  (treesit-font-lock-recompute-features))

(provide 'nvp-typescript)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-typescript.el ends here
