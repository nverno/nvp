;;; oat-mode.el --- Major mode for oat source -*- lexical-binding: t; -*-

;;; Commentary:

;; Derives from `cc-mode', so uses the c-lang interface to modify variables
;; inherited from `java-mode'.
;; 
;; Just using `java-mode' is pretty reasonable already, but this specializes
;; to oat's grammar and removes java's (or most of it).
;; Java handles the `int[] var` style syntax well
;;
;; Lang-dependent code in cc-font.el/cc-langs.el

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'cc-langs)
  (require 'cc-fonts))
(require 'cc-mode)

(defgroup oat nil
  "Major mode for editing Oat source files."
  :group 'languages
  :prefix "oat-")

(defcustom oat-indent-offset 2
  "Amount by which expressions are indented."
  :type 'integer
  :group 'oat)

(defcustom oat-font-lock-extra-types nil
  "List of extra types to recognize (regexps)."
  :type 'sexp
  :group 'oat)

;;; Modifications of java's defaults -- `c-lang-constants'
;; 
;; `c-constant-kwds' are fine => null, true, false
;; `c-paren-stmt-key' already handles vars in for loops

(eval-and-compile (c-add-language 'oat-mode 'java-mode))

(c-lang-defconst c-primitive-type-kwds oat '("void" "int" "string" "bool" "var"))
(c-lang-defconst c-simple-stmt-kwds oat '("return"))
(c-lang-defconst c-type-list-kwds oat '("new"))
;; (c-lang-defconst c-modifier-kwds oat '("global"))
(c-lang-defconst c-block-stmt-kwds oat '("else" "for" "if" "while"))
(c-lang-defconst c-keywords
  oat '("else" "false" "for" "global" "if" "new" "null" "return" "true" "void"
        "while"))
(c-lang-defconst c-arithmetic-operators
  oat '("*" "+" "-" "~" "<<" ">>" ">>>" "<" "<=" ">" ">=" "==" "!=" "&" "|"
        "[&]" "[|]"))

;; remove all these keywords from oat's font-locking
(eval-when-compile
  (defmacro oat:undef-c-consts (&rest kwds)
    (macroexp-progn
     (cl-loop for kwd in kwds
        collect `(c-lang-defconst ,kwd oat nil)))))

(oat:undef-c-consts
 c-inexpr-class-kwds
 c-primary-expr-kwds
 c-brace-list-decl-kwds
 c-before-label-kwds
 c-block-stmt-1-2-kwds
 c-ref-list-kwds
 c-label-kwds
 c-prefix-spec-kwds
 c-other-decl-kwds
 c-defun-type-name-decl-kwds
 c-case-kwds
 c-postfix-decl-spec-kwds
 c-ref-list
 c-modifier-kwds)

(defconst oat-font-lock-keywords-1 (c-lang-const c-matchers-1 oat))
(defconst oat-font-lock-keywords-2 (c-lang-const c-matchers-2 oat))
(defconst oat-font-lock-keywords-3 (c-lang-const c-matchers-3 oat))
(defvar oat-font-lock-keywords oat-font-lock-keywords-2
  "Default expressions to highlight in `oat-mode'.")

(defun oat-font-lock-keywords-2 ()
  (c-compose-keywords-list oat-font-lock-keywords-2))
(defun oat-font-lock-keywords-3 ()
  (c-compose-keywords-list oat-font-lock-keywords-3))
(defun oat-font-lock-keywords ()
  (c-compose-keywords-list oat-font-lock-keywords))

(defvar oat-mode-syntax-table nil)

;;;###autoload
(define-derived-mode oat-mode prog-mode "Oat"
  "Major mode for editing oat files.
The hook `c-mode-common-hook' is run with no args at mode
initialization, then `oat-mode-hook'.

\\{oat-mode-map}"
  :after-hook (c-update-modeline)
  :syntax-table c-mode-syntax-table     ; C-style comments

  ;; indentation
  (setq c-basic-offset oat-indent-offset)
  (make-local-variable 'c-offsets-alist)
  ;; (c-set-offset 'knr-argdecl-intro 0)

  ;; initialize cc-mode stuff
  (c-initialize-cc-mode t)
  (c-init-language-vars oat-mode)
  (c-common-init 'oat-mode)
  (c-run-mode-hooks 'c-mode-common-hook))

;;;###autoload(add-to-list 'auto-mode-alist '("\\.oat\\'" . oat-mode))

(provide 'oat-mode)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; oat-mode.el ends here
