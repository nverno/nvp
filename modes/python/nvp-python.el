;;; nvp-python.el --- python stuff -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(eval-when-compile (require 'nvp-macro))
(require 'python nil t)
(nvp:decls :p (python))

(nvp:defmethod nvp-parse-current-function ()
  :modes (python-mode python-ts-mode)
  (add-log-current-defun))

(with-eval-after-load 'nvp-repl
  (require 'nvp-python-repl))

(defvar nvp-python-breakpoint-string
  (cond ((executable-find "ipdb") "import ipdb; ipdb.set_trace()")
        ((executable-find "pudb") "import pudb; pudb.set_trace()")
        (t "import pdb; pdb.set_trace()"))
  "Breakpoint string to highlight.")

;; -------------------------------------------------------------------
;;; Tree-sitter mods

(defvar nvp-python-ts-font-lock-settings
  (treesit-font-lock-rules
   :language 'python
   :feature 'nvp
   ;; :override t
   `(;; Namespaces
     (import_from_statement
      module_name: [(dotted_name (identifier)) (identifier)]
      @font-lock-namespace-face)
     (import_statement
      name: [(aliased_import name: (dotted_name (identifier)))
             (dotted_name (identifier))]
      @font-lock-namespace-face)
     (aliased_import
      name: (dotted_name (identifier)) @font-lock-namespace-use-face)
     (aliased_import
      alias: (_) @font-lock-namespace-face)

     ;; method calls
     (attribute
      object: ((identifier) @font-lock-instance-ref-face
               (:match ,(rx bos "self" eos) @font-lock-instance-ref-face)))
     (attribute
      object: (identifier) @font-lock-receiver-face)
     (call
      function: (attribute attribute: (identifier) @font-lock-method-call-face))

     ;; Variables
     (keyword_argument
      name: (identifier) @font-lock-variable-name-face)

     (slice ":" @font-lock-operator-face)

     ;; Builtins
     ["__future__"] @font-lock-preprocessor-face

     ((identifier) @font-lock-builtin-face
      (:match ,(rx bos "__" (* (not white)) "__" eos) @font-lock-builtin-face)))))

(nvp:treesit-add-rules python-ts-mode
  :mode-lib python
  :new-fonts nvp-python-ts-font-lock-settings
  :mode-fonts python--treesit-settings)

;; -------------------------------------------------------------------
;;; Info

;;; TODO: for method lookup, need to transform symbol at point to be
;; prefixed by <module...><class>, eg.
;; "d.popleft" -> "collections.deque.popleft"
(declare-function info-lookup-add-help "info-look")
(with-eval-after-load 'info-look
  (let ((doc-spec
         ;; Note: info node will depend on python3-doc package installed
         ;; which may add the version suffix
         '(("(python3.10)Python Module Index")
           ;; ("(python3.10)Built-in Functions")
           ("(python3.10)Index"
            (lambda
              (item)
              (cond
               ;; module functions / variables
               ((string-match
                 "\\([A-Za-z0-9_]+\\)\\(?:()\\)? (in module \\([A-Za-z0-9_.]+\\))" item)
                (format "%s.%s" (match-string 2 item)
                        (match-string 1 item)))
               ;; builtins
               ((string-match
                 "built-in function; \\([A-Za-z][A-Za-z0-9]+\\)\\(?:()\\)?" item)
                (match-string 1 item))
               ;; class methods
               ((string-match
                 "\\([A-Za-z0-9_]+\\)() (\\([A-Za-z0-9_.]+\\) method)" item)
                (format "%s.%s" (match-string 2 item)
                        (match-string 1 item)))))))))
    (dolist (mode '(python-mode)) ; python-ts-mode
      (info-lookup-add-help
       :mode mode
       :regexp "[a-zA-Z_0-9.]+"
       :doc-spec doc-spec))))


(provide 'nvp-python)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-python.el ends here
