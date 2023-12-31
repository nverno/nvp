;;; nvp-ecma.el --- ECMA stuff for js/typescript -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Font-locking shared b/w javascript and typescript
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp:decls)

(defvar ecma-ts-builtin-variables
  '("arguments" "module" "console" "window" "document"))

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

(defface nvp-interpreter-face
  '((t (:inherit font-lock-keyword-face :italic t)))
  "Face for shebang interpreter.")

(defun ecma-ts--fontify-hash-bang (node override start end &rest _)
  (save-excursion
    (goto-char (treesit-node-start node))
    (save-match-data
      (when (looking-at
             "\\`\\(#!\\).*/\\([^ \t\n]+\\)\\(?:[ \t]+\\([^ \t\n]+\\)\\)?")
        (let* ((m (if (and (string= "env" (buffer-substring-no-properties
                                           (match-beginning 2) (match-end 2)))
                           (match-beginning 3))
                      3
                    2))
               (b (match-beginning m))
               (e (match-end m)))
          (treesit-fontify-with-override
           b e 'font-lock-comment-delimiter-face override start end)
          (treesit-fontify-with-override
           b e 'nvp-interpreter-face override start end))))))

;;;###autoload
(defun ecma-ts-font-lock-rules (lang)
  (list
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
      @font-lock-builtin-face
      ((method_definition
        name: (property_identifier) @font-lock-keyword-face)
       (:match "\\`constructor\\'" @font-lock-keyword-face)))
    :language lang
    :feature 'property
    `((property_signature
       name: (property_identifier) @font-lock-property-name-face)
      (public_field_definition
       name: (property_identifier) @font-lock-property-name-face)
      (index_signature
       name: (identifier) @font-lock-property-name-face)
      (pair
       key: (_) @font-lock-property-use-face
       value: (identifier) @font-lock-variable-use-face)
      ((shorthand_property_identifier) @font-lock-property-use-face)))

   ;; Appended after rest of rules
   (treesit-font-lock-rules
    :language lang
    :feature 'preproc
    :override t
    '((hash_bang_line) @ecma-ts--fontify-hash-bang
      ((string_fragment) @font-lock-preprocessor-face
       (:match "\\`use strict\\'" @font-lock-preprocessor-face))
      (decorator "@" @font-lock-operator-face
                 [(call_expression (identifier)) (identifier)]
                 @font-lock-preprocessor-face)))))


(provide 'nvp-ecma)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-ecma.el ends here
