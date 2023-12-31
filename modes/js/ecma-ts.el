;;; ecma-ts.el --- ECMA treesit font-lock for js/jsx/ts/tsx -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Font-locking shared b/w javascript, jsx, typescript, tsx...
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls)

(defface ecma-ts-interpreter-face
  '((t (:inherit font-lock-keyword-face :italic t)))
  "Face for shebang interpreter.")

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

(defun ecma-ts--fontify-hash-bang (node override start end &rest _)
  (save-excursion
    (goto-char (treesit-node-start node))
    (save-match-data
      (when (looking-at
             "\\`\\(#!\\).*/\\([^ \t\n]+\\)\\(?:[ \t]+\\([^ \t\n]+\\)\\)?")
        (let ((m (if (and (string= "env" (buffer-substring-no-properties
                                          (match-beginning 2) (match-end 2)))
                          (match-beginning 3))
                     3
                   2)))
          (treesit-fontify-with-override
           (match-beginning 1) (match-end 1)
           'font-lock-comment-delimiter-face override start end)
          (treesit-fontify-with-override
           (match-end 1) (match-beginning m)
           'font-lock-comment-face override start end)
          (treesit-fontify-with-override
           (match-beginning m) (match-end m)
           'ecma-ts-interpreter-face override start end))))))

(defun ecma-ts--fontify-comment (node override start end &rest _)
  (let ((txt (treesit-node-text node))
        (b (treesit-node-start node))
        (e (treesit-node-end node)))
    (if (and (string-prefix-p "/**" txt)
             (string-suffix-p "*/" txt))
        (treesit-fontify-with-override b e 'font-lock-doc-face override start end)
      (treesit-fontify-with-override
       b (+ 2 b) 'font-lock-comment-delimiter-face override start end)
      (treesit-fontify-with-override
       (+ 2 b) e 'font-lock-comment-face override start end))))

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
    `((hash_bang_line) @ecma-ts--fontify-hash-bang
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
      [(undefined) (null)] @font-lock-type-face))

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
    '((hash_bang_line) @ecma-ts--fontify-hash-bang
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
      (property_identifier) @font-lock-property-use-face))))

;;;###autoload
(defun ecma-ts-merge-rules (language rules)
  (pcase-let* ((`(,hd . ,tl) (ecma-ts-font-lock-rules language))
               (features (--map (nth 2 it) (append hd tl)))
               (rules (--filter (not (memq (nth 2 it) features)) rules)))
    (append hd rules tl)))

(provide 'ecma-ts)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; ecma-ts.el ends here
