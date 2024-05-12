;;; nvp-typescript.el --- typescript -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls :p (typescript ecma))

(eval-and-compile
  (defvar nvp-typescript-modes
    '(typescript-mode typescript-ts-mode typescript-tsx-mode tsx-ts-mode)))

(nvp:defmethod nvp-newline-dwim-comment (syntax arg)
  :modes nvp-typescript-modes
  (nvp-newline-dwim--comment syntax arg " * "))

(with-eval-after-load 'nvp-repl
  (require 'nvp-typescript-repl))

;; -------------------------------------------------------------------
;;; `typescript-ts-mode'
(require 'typescript-ts-mode nil t)

(defun typescript-ts-mode--compile-assignment-query (lang)
  "Compile query that captures object, index, and property identifiers."
  (when (treesit-available-p)
    (treesit-query-compile
     lang
     '((subscript_expression object: [(this) (identifier)] @id)
       (member_expression object: [(this) (identifier)] @id)
       (member_expression property: (property_identifier) @property)))))

(defvar typescript-ts-mode--assignment-lhs-query
  (typescript-ts-mode--compile-assignment-query 'typescript))

(defvar tsx-ts-mode--assignment-lhs-query
  (typescript-ts-mode--compile-assignment-query 'tsx))

(defvar-local typescript-ts-mode--assignment-query
    typescript-ts-mode--assignment-lhs-query)

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

(defvar nvp-typescript-ts-font-lock-rules nil)
(defvar nvp-tsx-ts-font-lock-rules nil)

(defun nvp-typescript-ts-font-lock-rules (rules-fn language)
  (when (eq 'tsx language)
    (setq-local typescript-ts-mode--assignment-query
                tsx-ts-mode--assignment-lhs-query))
  (let ((v (intern (format "nvp-%s-ts-font-lock-rules" language))))
    (or (and nil nvp-typescript-ts-font-lock-rules)
        (let ((new-rules
               (treesit-font-lock-rules
                :language language
                :feature 'nvp
                ;; TODO(5/2/24): patch
                '(;; Type signatures
                  (property_signature
                   name: (property_identifier) @font-lock-property-name-face)
                  (index_signature
                   name: (identifier) @font-lock-property-name-face)

                  ;; Missing class field definitions:
                  ;; class ... {
                  ;;   func = (...) => { ... }
                  ;; }
                  (public_field_definition
                   name: [(private_property_identifier) (property_identifier)]
                   @font-lock-function-name-face
                   value: (arrow_function))
                  (public_field_definition
                   name: [(private_property_identifier) (property_identifier)]
                   @font-lock-property-name-face
                   value: (_))

                  ;; Missing call: this.#_func()
                  (call_expression
                   function:
                   (member_expression
                    property: [(private_property_identifier) (property_identifier)]
                    @font-lock-function-call-face))

                  ;; Missing operators: "?."
                  [(optional_chain)] @font-lock-operator-face

                  ;; declare class C {
                  ;;   combine(...those: Psbt[]): this;
                  ;; }
                  (this_type) @font-lock-type-face
                  (required_parameter
                   (rest_pattern (identifier) @font-lock-variable-name-face)))
                
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
                :feature 'ts-extension
                '(["?:"] @font-lock-delimiter-face)
                
                :language language
                :feature 'namespace
                '(["module" "global"] @font-lock-keyword-face
                  (internal_module
                   name: (identifier) @font-lock-type-face)
                  (internal_module
                   name: (nested_identifier
                          [(identifier) (property_identifier)]
                          @font-lock-type-face))
                  (module
                   name: (identifier) @font-lock-function-name-face)
                  (module
                   name: (nested_identifier
                          [(identifier) (property_identifier)]
                          @font-lock-function-name-face)))))
              (old-rules (funcall rules-fn language)))
          (set v (ecma-ts-merge-rules language (append old-rules new-rules)))))))

;; -------------------------------------------------------------------
;;; Add changes

(advice-add 'typescript-ts-mode--font-lock-settings
            :around #'nvp-typescript-ts-font-lock-rules)

;;; Add missing features once
(nvp:treesit-add-rules typescript-ts-mode
  :extra-features '(variable builtin namespace assignment preproc nvp))

(nvp:treesit-add-rules tsx-ts-mode
  :extra-features '(operator variable builtin namespace assignment preproc))

(provide 'nvp-typescript)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-typescript.el ends here
