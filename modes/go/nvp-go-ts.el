;;; nvp-go-ts.el --- Go tree-sitter mods -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'go-ts-mode nil t)
(nvp:decls)

;;; Indentation:
;; Modify to indent /* ... */
(require 'c-ts-common)                  ; comment indentation + filling
(defvar nvp-go-ts--indent-rules
  `(((and (parent-is "comment") c-ts-common-looking-at-star)
     c-ts-common-comment-start-after-first-star -1)
    ((parent-is "comment") prev-adaptive-prefix 0))
  "Additional indent rules to handle comments.")

;;; Font-locking
(defvar nvp-go-ts--builtin-types
  '("any" "bool" "byte" "comparable" "complex128" "complex64" "error" "float32"
    "float64" "int" "int16" "int32" "int64" "int8" "rune" "string" "uint"
    "uint16" "uint32" "uint64" "uint8" "uintptr"))

(defvar nvp-go-ts-font-lock-settings
  ;; (treesit-font-lock-rules
  ;;  :language 'go
  ;;  :feature 'namespace
  ;;  '((call_expression
  ;;     function: (selector_expression
  ;;                operand: (identifier) @nvp-namespace-use-face))))
  nil)

;; Add additional font-locking, indentation, and remove 'error feature
(with-eval-after-load 'go-ts-mode
  (cl-pushnew ":" go-ts-mode--operators :test #'equal)
  (let* ((features (--map (nth 2 it) nvp-go-ts-font-lock-settings))
         (rules (--filter (not (memq (nth 2 it) (cons 'error features)))
                          go-ts-mode--font-lock-settings)))
    (setq go-ts-mode--font-lock-settings
          (append nvp-go-ts-font-lock-settings rules))
    (setq go-ts-mode--indent-rules
          `((go ,@(append nvp-go-ts--indent-rules
                          (assoc-default 'go go-ts-mode--indent-rules)))))))

(nvp:run-once go-ts-mode (:after (&rest _))
  (dolist (v (--map (nth 2 it) nvp-go-ts-font-lock-settings))
    (cl-pushnew v (cadddr treesit-font-lock-feature-list))))

(provide 'nvp-go-ts)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-go-ts.el ends here
