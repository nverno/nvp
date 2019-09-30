;;; llvm-mode.el --- Major mode for the LLVM assembler language.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Major mode for editing LLVM IR files.

;; Modified from https://github.com/llvm-mirror/llvm//utils/emacs/llvm-mode.el
;; to include
;; - additional syntax
;; - font-lock for globals (vars/declares/defines)
;; - imenu
;; - indentation: `llvm-mode-indent-offset' and `llvm-mode-label-offset'
;; - completion:
;;  + global variables
;;  + global declares/defines
;; TODO:
;;  + keywords / attributes
;;  + could add labels / %uids as well
;;
;; - vim syntax => https://github.com/llvm-mirror/llvm/utils/vim/syntax/llvm.vim

;; Reference:
;; https://github.com/llvm-mirror/llvm/docs/LangRef.rst

;;; Code:
(require 'smie)

(defgroup llvm-mode nil
  "Major mode for editing llvm assembly source code."
  :group 'languages
  :prefix "llvm-mode-")

(defcustom llvm-mode-indent-offset 2
  "Indentation column following opening braces."
  :group 'llvm-mode
  :type 'integer)

(defcustom llvm-mode-label-offset 0
  "Indentation column for labels."
  :group 'llvm-mode
  :type 'integer)

;; Emacs 23 compatibility.
(defalias 'llvm-mode-prog-mode
  (if (fboundp 'prog-mode)
      'prog-mode
    'fundamental-mode))

(defvar llvm-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?% "_" table)
    (modify-syntax-entry ?. "_" table)
    (modify-syntax-entry ?\; "< " table)
    (modify-syntax-entry ?\n "> " table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?* "." table)
    table)
  "Syntax table used while in LLVM mode.")

(defconst llvm-font-lock-keywords
  (list
   ;; Attributes
   `(,(regexp-opt
       '("alwaysinline" "argmemonly" "builtin" "cold" "convergent" "immarg"
         "inaccessiblemem_or_argmemonly" "inaccessiblememonly" "inlinehint"
         "jumptable" "minsize" "naked" "nobuiltin" "noduplicate"
         "noimplicitfloat" "noinline" "nonlazybind" "norecurse" "noredzone"
         "noreturn" "nounwind" "optnone" "optsize" "readnone" "readonly"
         "returns_twice" "safestack" "sanitize_address" "sanitize_hwaddress"
         "sanitize_memory" "sanitize_memtag" "sanitize_thread" "speculatable"
         "ssp" "sspreq" "sspstrong" "strictfp" "uwtable" "writeonly")
       'symbols)
     . font-lock-constant-face)
   ;; Globals
   '("@[[:alnum:]_]+" . font-lock-function-name-face)
   ;; Variables
   '("%[-a-zA-Z$._][-a-zA-Z$._0-9]*" . font-lock-variable-name-face)
   ;; Labels
   '("[-a-zA-Z$._0-9]+:" . font-lock-variable-name-face)
   ;; Unnamed variable slots
   '("%[-]?[0-9]+" . font-lock-variable-name-face)
   ;; Types
   `(,(regexp-opt
       '("void" "i1" "i8" "i16" "i32" "i64" "i128" "float" "double" "type"
         "label" "opaque")
       'symbols)
     . font-lock-type-face)
   ;; Integer literals
   '("\\b[-]?[0-9]+\\b" . font-lock-preprocessor-face)
   ;; Floating point constants
   '("\\b[-+]?[0-9]+.[0-9]*\\([eE][-+]?[0-9]+\\)?\\b" . font-lock-preprocessor-face)
   ;; Hex constants
   '("\\b0x[0-9A-Fa-f]+\\b" . font-lock-preprocessor-face)
   ;; Keywords
   `(,(regexp-opt
       '(;; Toplevel entities
         "declare" "define" "module" "target" "source_filename" "global"
         "constant" "const" "attributes" "uselistorder" "uselistorder_bb"
         ;; Linkage types
         "private" "internal" "weak" "weak_odr" "linkonce" "linkonce_odr"
         "available_externally" "appending" "common" "extern_weak" "external"
         "uninitialized" "implementation" "..."
         ;; Values
         "true" "false" "null" "undef" "zeroinitializer" "none" "c" "asm"
         "blockaddress"
         ;; Calling conventions
         "ccc" "fastcc" "coldcc" "webkit_jscc" "anyregcc" "preserve_mostcc"
         "preserve_allcc" "cxx_fast_tlscc" "swiftcc" "atomic" "volatile"
         "personality" "prologue" "section")
       'symbols)
     . font-lock-keyword-face)
   ;; Arithmetic and Logical Operators
   `(,(regexp-opt
       '("add" "sub" "mul" "sdiv" "udiv" "urem" "srem" "and" "or" "xor" "setne"
         "seteq" "setlt" "setgt" "setle" "setge")
       'symbols)
     . font-lock-keyword-face)
   ;; Floating-point operators
   `(,(regexp-opt '("fadd" "fsub" "fneg" "fmul" "fdiv" "frem") 'symbols)
     . font-lock-keyword-face)
   ;; Special instructions
   `(,(regexp-opt
       '("phi" "tail" "call" "select" "to" "shl" "lshr" "ashr" "fcmp" "icmp"
         "va_arg" "landingpad")
       'symbols)
     . font-lock-keyword-face)
   ;; Control instructions
   `(,(regexp-opt
       '("ret" "br" "switch" "invoke" "resume" "unwind" "unreachable"
         "indirectbr")
       'symbols)
     . font-lock-keyword-face)
   ;; Memory operators
   `(,(regexp-opt
       '("malloc" "alloca" "free" "load" "store" "getelementptr" "fence"
         "cmpxchg" "atomicrmw")
       'symbols)
     . font-lock-keyword-face)
   ;; Casts
   `(,(regexp-opt
       '("bitcast" "inttoptr" "ptrtoint" "trunc" "zext" "sext" "fptrunc" "fpext"
         "fptoui" "fptosi" "uitofp" "sitofp" "addrspacecast")
       'symbols)
     . font-lock-keyword-face)
   ;; Vector ops
   `(,(regexp-opt '("extractelement" "insertelement" "shufflevector") 'symbols)
     . font-lock-keyword-face)
   ;; Aggregate ops
   `(,(regexp-opt '("extractvalue" "insertvalue") 'symbols) . font-lock-keyword-face)
   ;; Metadata types
   `(,(regexp-opt '("distinct") 'symbols) . font-lock-keyword-face)
   ;; Use-list order directives
   `(,(regexp-opt '("uselistorder" "uselistorder_bb") 'symbols)
     . font-lock-keyword-face))
  "Syntax highlighting for LLVM.")


;;; Indentation

(defconst llvm-mode-smie-grammar
  (smie-prec2->grammar
   (smie-precs->prec2
    '((assoc ":")))))

;; return ":" on label line 
(defun llvm-mode--smie-forward-token ()
  (let ((tok (smie-default-forward-token)))
    (save-match-data
      (if (not (looking-at "[ \t]*:")) tok
        (goto-char (match-end 0))
        ":"))))

(defun llvm-mode-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem       . basic) llvm-mode-indent-offset)
    (`(:elem       . args) 0)
    (`(:close-all  . ,_) t)
    (`(:before     . ":")
     (if (smie-rule-parent-p ":") 0
       llvm-mode-label-offset))
    (`(:after      . ":")
     (if (smie-rule-prev-p ":") llvm-mode-indent-offset
       (- llvm-mode-indent-offset llvm-mode-label-offset)))
    (`(:list-intro . ,(or ":" "")) t)))


;;; Completion

(defconst llvm-mode-global-regexp
  (concat "^\\s-*" (regexp-opt '("declare" "define")) "\\s-*"
          "[^@\n]+@\\([[:alnum:]_]+\\)"
          "\\|^\\s-*@\\([[:alnum:]_]+\\) *="))

(defun llvm-mode-globals ()
  (let (res)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward llvm-mode-global-regexp nil t)
        (push (or (match-string-no-properties 1)
                  (match-string-no-properties 2))
              res)))
    res))

;; basic completion at point 
(defun llvm-mode-completion-at-point ()
  (when-let ((bnds (bounds-of-thing-at-point 'symbol)))
    (let ((table
           (cond
            ((eq ?@ (char-before (car bnds)))
             (list
              (completion-table-with-cache
               (lambda (_string) (llvm-mode-globals)))
              :annotation-function (lambda (_s) " <g>"))))))
      (when table
        (nconc (list (car bnds) (cdr bnds))
               table
               (list :exclusive 'no))))))

;; defines / declares / labels
;; XXX: remove duplicate labels??
(defvar llvm-mode-imenu-regexp
  `((nil
     ,(concat (regexp-opt '("declare" "define")) "[^@\n]+@\\([[:alnum:]_]+\\)")
     1)
    ("Label" "^\\s-*\\([[:alpha:]][[:alnum:]_]*\\):" 1)))

;;;###autoload
(define-derived-mode llvm-mode llvm-mode-prog-mode "LLVM"
  "Major mode for editing LLVM source files.
\\{llvm-mode-map}
  Runs `llvm-mode-hook' on startup."
  (setq font-lock-defaults `(llvm-font-lock-keywords))
  (setq-local comment-start ";")
  (setq-local imenu-generic-expression llvm-mode-imenu-regexp)
  (add-hook 'completion-at-point-functions #'llvm-mode-completion-at-point nil t)
  (smie-setup llvm-mode-smie-grammar #'llvm-mode-smie-rules
              :forward-token #'llvm-mode--smie-forward-token
              :backward-token #'smie-default-backward-token))

;; Associate .ll files with llvm-mode
;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.ll\\'" 'llvm-mode))

(provide 'llvm-mode)

;;; llvm-mode.el ends here
