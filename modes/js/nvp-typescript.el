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

;; Tree-sitter version of `typescript-autoconvert-to-template'
(defun typescript-ts-convert-to-template (&optional interactive)
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

(defun typescript-ts--post-self-insert-function ()
  "Auto convert strings to templates."
  (cl-assert (eq major-mode 'typescript-ts-mode))
  (when (and (eq ?\{ last-command-event)
             (eq ?$ (char-before (1- (point))))
             (memq (nth 3 (syntax-ppss)) '(?\' ?\")))
    (typescript-ts-convert-to-template)))

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
(nvp:run-once typescript-ts-mode (:after (&rest _))
  (dolist (v '(variable builtin namespace assignment preproc))
    (cl-pushnew v (cadddr treesit-font-lock-feature-list)))
  (treesit-font-lock-recompute-features))

(nvp:run-once tsx-ts-mode (:after (&rest _))
  (dolist (v '(operator variable builtin namespace assignment preproc))
    (cl-pushnew v (cadddr treesit-font-lock-feature-list)))
  (treesit-font-lock-recompute-features))

(provide 'nvp-typescript)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-typescript.el ends here
