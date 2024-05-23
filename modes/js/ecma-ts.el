;;; ecma-ts.el --- ECMA treesit font-lock for js/jsx/ts/tsx -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Font-locking shared b/w javascript, jsx, typescript, tsx...
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)                          ; `nvp-receiver-face'
(nvp:decls)

(defvar ecma-ts-builtin-variables
  '("arguments" "module" "console" "window" "document" "self"))

(defvar ecma-ts-builtin-types
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

(defvar ecma-ts-builtin-functions
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

(defvar ecma-ts-operators
  ;; Note: typescript only: "?."
  '("--" "-" "-=" "&&" "+" "++" "+=" "&=" "/=" "**=" "<<=" "<" "<=" "<<" "="
    "==" "===" "!=" "!==" "=>" ">" ">=" ">>" "||" "%" "%=" "*" "**" ">>>" "&"
    "/" "%" "|" "^" "??" "*=" ">>=" ">>>=" "^=" "|=" "&&=" "||=" "??=" "..."
    "?" "!" "~"))

(defun ecma-ts--fontify-comment (node override start end &rest _)
  (let* ((txt (treesit-node-text node))
         (b (treesit-node-start node))
         (e (treesit-node-end node))
         (multiline-p (and (string-prefix-p "/*" txt) (string-suffix-p "*/" txt)))
         (doc-p (and multiline-p (string-prefix-p "/**" txt))))
    (treesit-fontify-with-override
     b e (if doc-p 'font-lock-doc-face 'font-lock-comment-face)
     override start end)
    (if (not multiline-p)
        (treesit-fontify-with-override
         b (+ 2 b) 'font-lock-comment-delimiter-face 'append start end)
      (treesit-fontify-with-override
       b (+ (if doc-p 3 2) b) 'font-lock-comment-delimiter-face 'append start end)
      (treesit-fontify-with-override
       (- e 2) e 'font-lock-comment-delimiter-face 'append start end))))

;;;###autoload
(defun ecma-ts-font-lock-rules (lang)
  (cons
   (treesit-font-lock-rules
    :language lang
    :feature 'builtin
    `(((identifier) @_var
       (:match ,(rx-to-string
                 `(seq bos (or ,@ecma-ts-builtin-variables
                               ,@ecma-ts-builtin-types
                               ,@ecma-ts-builtin-functions)
                       eos))
               @_var))
      @font-lock-builtin-face)

    :language lang
    :feature 'comment
    `((hash_bang_line) @nvp-treesit-fontify-hash-bang
      ((comment) @ecma-ts--fontify-comment))

    :feature 'number
    :language lang
    `((number) @font-lock-number-face
      ((identifier) @font-lock-number-face
       (:match "\\`\\(?:NaN\\|Infinity\\)\\'" @font-lock-number-face)))

    :language lang
    :feature 'string
    '([(regex_pattern) (regex_flags)] @font-lock-regexp-face
      (regex "/" @font-lock-bracket-face)
      (string) @font-lock-string-face
      (template_string) @js--fontify-template-string
      (template_substitution ["${" "}"] @font-lock-misc-punctuation-face))

    :language lang
    :feature 'operator
    `([,@ecma-ts-operators] @font-lock-operator-face
      (ternary_expression ["?" ":"] @font-lock-operator-face))

    :language lang
    :feature 'delimiter
    '((["," "." ";" ":"]) @font-lock-delimiter-face)

    :language lang
    :feature 'bracket
    '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

    :language lang
    :feature 'constant
    `(((identifier) @font-lock-constant-face
       (:match "\\`[A-Z_][0-9A-Z_]*\\'" @font-lock-constant-face))
      [(true) (false) (statement_identifier)]
      @font-lock-constant-face
      ;; FIXME: move types or other feature
      [(undefined) (null)] @font-lock-type-face)

    :language lang
    :feature 'expression
    '((assignment_expression
       left: [(identifier) @font-lock-function-name-face
              (member_expression
               property: (property_identifier) @font-lock-function-name-face)]
       right: [(function_expression) (arrow_function)])
      (variable_declarator
       name: (identifier) @font-lock-function-name-face
       value: [(function_expression) (arrow_function)])))

   ;; Appended after rest of rules
   (treesit-font-lock-rules
    :language lang
    :feature 'property
    `(;; (property_signature
      ;;  name: (property_identifier) @font-lock-property-name-face)
      ;; (public_field_definition
      ;;  name: (property_identifier) @font-lock-property-name-face)
      ;; (index_signature
      ;;  name: (identifier) @font-lock-property-name-face)
      ;; (shorthand_property_identifier) @font-lock-variable-name-face
      [(shorthand_property_identifier) (shorthand_property_identifier_pattern)]
      @font-lock-variable-name-face
      (pair key: (_) @font-lock-property-use-face
            value: (identifier) @font-lock-variable-use-face))

    :language lang
    :feature 'preproc
    :override t
    '(;; (hash_bang_line) @nvp-treesit-fontify-hash-bang
      ((string_fragment) @font-lock-preprocessor-face
       (:match "\\`use strict\\'" @font-lock-preprocessor-face))
      (decorator "@" @font-lock-preprocessor-face
                 [(call_expression (identifier)) (identifier)]
                 @font-lock-preprocessor-face)
      ;; override string keys
      (pair key: (_) @font-lock-property-name-face)
      ;; Namespaces
      (((identifier) @font-lock-type-face
        (:match "Intl" @font-lock-type-face)))
      (namespace_import
       (identifier) @font-lock-type-face)
      ;; Class Constructor
      ((method_definition
        name: (property_identifier) @font-lock-keyword-face)
       (:match "\\`constructor\\'" @font-lock-keyword-face)))

    :language lang
    :feature 'variable
    '((identifier) @font-lock-variable-use-face
      (member_expression
       object: (identifier) @nvp-receiver-face)
      (property_identifier) @font-lock-property-use-face))))

;;;###autoload
(defun ecma-ts-merge-rules (language rules)
  (pcase-let* ((`(,hd . ,tl) (ecma-ts-font-lock-rules language))
               (features (--map (nth 2 it) (append hd tl)))
               (rules (--filter (not (memq (nth 2 it) features)) rules)))
    (append hd rules tl)))

;;; Auto-convert strings to templates
;; Tree-sitter version of `typescript-autoconvert-to-template'
(defun ecma-ts-convert-to-template (&optional interactive)
  "Automatically convert a plain string to a teplate string."
  (interactive (list t))
  (when-let* ((node (treesit-node-at (point)))
              (str-node (pcase (treesit-node-type node)
                          ((or "\"" "'" "string_fragment")
                           (treesit-node-parent node))
                          ("string" node)
                          (_ nil)))
              (beg (treesit-node-start str-node))
              (end (treesit-node-end str-node)))
    (save-excursion
      (when (or (not interactive)
                (progn (goto-char beg)
                       (re-search-forward "\\${.*?}" end t))
                (user-error "Dont think it's a string"))
        (goto-char beg)
        (delete-char 1)
        (insert "`")
        (goto-char end)
        (delete-char -1)
        (insert "`")))))

;;;###autoload
(defun ecma-ts--post-self-insert-function ()
  "Auto convert strings to templates."
  (cl-assert (memq major-mode '(js-ts-mode typescript-ts-mode)))
  (when (and (eq ?\{ last-command-event)
             (eq ?$ (char-before (1- (point))))
             (memq (nth 3 (syntax-ppss)) '(?\' ?\")))
    (ecma-ts-convert-to-template)))

(provide 'ecma-ts)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; ecma-ts.el ends here
