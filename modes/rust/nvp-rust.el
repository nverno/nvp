;;; nvp-rust.el --- rustls -*- lexical-binding: t; -*-
;;; Commentary:
;; TODO:
;; - macroexp: rustc --pretty expanded, or rustc --pretty expanded,hygiene
;;   https://doc.rust-lang.org/book/macros.html#debugging-macro-code
;; - deps
;;; Code:
(eval-when-compile (require 'nvp-macro))
;; (require 'rust-mode)
(require 'rustic nil t)
(nvp:decls :v (toml-mode-map) :f (rustic-cargo-current-test))
(nvp:auto 'rustic-cargo rustic-cargo--get-test-target)

(cl-defmethod nvp-newline-dwim-comment
  (syntax arg &context (major-mode rust-mode))
  (nvp-newline-dwim--comment syntax arg " * "))

(cl-defmethod nvp-newline-dwim-comment
  (syntax arg &context (major-mode rustic-mode))
  (nvp-newline-dwim--comment syntax arg " * "))

(defun nvp-rust-current-test-dwim ()
  (interactive)
  (if (rustic-cargo--get-test-target)
      (rustic-cargo-current-test)
    (when (fboundp 'lsp-rust-analyzer-related-tests)
      (call-interactively #'lsp-rust-analyzer-related-tests))))

;; -------------------------------------------------------------------
;;; Insert / Toggle

;; rust mode functions:
;; rust-in-comment-paragraph
;; rust-in-macro
;; rust-in-str-or-cmnt
;; rust-beginning-or-defun
;; rust-end-of-defun

;; toggle public visibility of function at point
;; (defun nvp-rust-toggle-pub ()
;;   (interactive)
;;   (save-excursion
;;     (rust-beginning-of-defun)
;;     (if (and (looking-at-p "pub ")
;;              (message "pub off"))
;;         (delete-char 4)
;;       (insert "pub ")
;;       (message "pub on"))))

;; toggle mutability of variable at point
;; (defun nvp-rust-toggle-mut ()
;;   (interactive)
;;   (save-excursion
;;     (racer-find-definition)
;;     (back-to-indentation)
;;     (forward-char 4)
;;     (if (and (looking-at-p "mut ")
;;              (message "mutability off"))
;;         (delete-char 4)
;;       (insert "mut ")
;;       (message "mutability on"))))

;;; Enums / Struct

;; (eval-when-compile
;;   ;; do body business at item location
;;   (defmacro nvp-rust-at-definition-of (item &rest body)
;;     (declare (indent defun))
;;     `(save-excursion
;;        (with-current-buffer (find-file-noselect
;;                              (get-text-property 0 'file ,item))
;;          (goto-char (point-min))
;;          (forward-line (1- (get-text-property 0 'line ,item)))
;;          (forward-char (get-text-property 0 'col ,item))
;;          ,@body)))

;;   ;; collect stuff between beginning/end of rust def at point
;;   (defmacro nvp-rust-collect-fields (regexp)
;;     (declare (indent defun))
;;     `(save-excursion
;;        (rust-beginning-of-defun)
;;        (let ((end (save-excursion
;;                     (rust-end-of-defun)
;;                     (point)))
;;              (case-fold-search)
;;              opts)
;;          (while (< (point) end)
;;            (and (looking-at ,regexp)
;;                 (push (match-string-no-properties 1) opts))
;;            (forward-line 1))
;;          opts))))

;; find type in let expression
(defun nvp-rust-let-type ()
  (let (case-fold-search)
    ;; "\\_<\\([[:upper:]][_[:digit:][:nonascii:][:word:]]*\\)\\_>"
    (when (re-search-forward
           rust-re-type-or-constructor (line-end-position) t)
      (match-string 1))))

;; Get cases of thing at point (|):
;;
;; Option| ==> (:enum "Some" "None")
;;
;; fn blah(t: &Option<&Thing>) -> Thing {
;;   match *t| ==> (:enum "Some" "None")
;;
;; let x = Some("value");
;; x| ==> (:enum "Some" "None")
;;
;; let x: Option<u32> = ...
;; x| ==> (:enum "Some" "None")
;;
;;; struct
;;
;; struct Point {
;;   x: i32,
;;   y: i32,
;; }
;; let point = Point| ==> (:struct "x" "y")
;;
;; FIXME: fix reading fields, matches by line ...

;; (defun nvp-rust-cases ()
;;   (-when-let* ((sym (symbol-at-point))
;;                (sym (cl-find-if
;;                      (lambda (s) (string= sym s)) (racer-complete))))
;;     (pcase (get-text-property 0 'matchtype sym)
;;       ((or "Let" "FnArg")
;;        (nvp-rust-at-definition-of sym
;;          (when (nvp-rust-let-type)
;;            (nvp-rust-cases))))
;;       ((or "Enum" "EnumVariant")
;;        (nvp-rust-at-definition-of sym
;;          (cons :enum (nvp-rust-enum-fields))))
;;       ("Struct"
;;        (nvp-rust-at-definition-of sym
;;          (cons :struct (nvp-rust-struct-fields))))
;;       (_ sym))))

;; get fields of enum at point
;; (defun nvp-rust-enum-fields ()
;;   (nvp-rust-collect-fields
;;     (concat "^\\s-*" rust-re-type-or-constructor)))

;; struct fields
;; (defun nvp-rust-struct-fields ()
;;   (nvp-rust-collect-fields "^\\s-*\\([_a-z0-9]+\\):"))

;; get fields for struct / enum we be at
;; (defun nvp-rust-match ()
;;   (interactive)
;;   (let ((_meta (racer--call-at-point "find-definition")))
;;     ;; FIXME:
;;     ))

(provide 'nvp-rust)
;;; nvp-rust.el ends here
