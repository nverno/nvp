;;; nvp-c.el --- c helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)                          ; `nvp-receiver-face'
(nvp:decls
 :f ( forward-ifdef nvp-yas-var s-join
      c-syntactic-information-on-region c-show-syntactic-information)
 :v (c/R-abbrev-table company-clang-arguments gud-comint-buffer))
(nvp:auto "nvp-tag" 'nvp-tag-list-decls)


(defvar-local nvp-c-local-include-paths '("." ".." "../include"))

(nvp:defmethod nvp-newline-dwim-comment (syntax arg)
  :modes (c-mode c-ts-mode)
  (nvp-newline-dwim--comment syntax arg " * "))

(defun nvp-c-show-syntactic-information ()
  (interactive)
  ;; TODO: remove comments added by `c-syntactic-information-on-region' with
  ;; prefix
  (call-interactively
   (if (region-active-p)
       #'c-syntactic-information-on-region
     #'c-show-syntactic-information)))

;;; Macroexpansion
;; can set in .dir-locals
;; (put 'c-macro-cppflags 'safe-local-variable #'stringp)
;; (put 'c-macro-preprocessor 'safe-local-variable #'stringp)

(defun nvp-c-macro-cppflags (&optional extra lang clang)
  "Create flags for preprocessor.
EXTRA will be appended as -I.
LANG should have known system includes, eg. c/c++.
CLANG if non-nil use clang system includes."
  (let* ((lang (or (and lang (symbol-name lang)) "c++"))
         (var (intern-soft (format "nvp-%s%s-include-dirs" (if clang "clang-" "") lang)))
         (sysincludes (cl-remove-if (lambda (s) (string-prefix-p "." s))
                                    (symbol-value var))))
    (if (null sysincludes)
        (user-error "%S includes not defined" lang)
      (concat "-x" lang " -std=c++11 "
              (mapconcat (lambda (s) (concat "-isystem " s)) sysincludes " ")
              (mapconcat (lambda (s) (concat "-I" s)) extra " ")))))

;;; C Style
(defun nvp-c-style-from-clang-format (&optional prog)
  "Determine C style format variables from clang-format.
Return list like \\='((indent-tabs-mode . t) (c-basic-offset . 2) ...)."
  (let ((prog (or prog "clang-format")))
    (if (and (executable-find prog) (executable-find "yq"))
        (let* ((conf (shell-command-to-string
                      (format "%s -dump-config | yq '.IndentWidth,.UseTab,.BasedOnStyle'"
                              (or prog "clang-format")))))
          (cl-destructuring-bind (indent tabs style &rest args) (split-string conf "\n")
            (message "%S %S %S" indent tabs style)
            (let ((indent (string-to-number indent))
                  (tabs (not (or (string-empty-p tabs)
                                 (string-equal-ignore-case "Never" tabs))))
                  res)
              (when (> indent 0) (push (cons 'c-basic-offset indent) res))
              (push (cons 'indent-tabs-mode tabs) res)
              res)))
      (user-error "%s and/or yq not found" prog))))

;;; Abbrevs
(defun nvp-c-abbrev-expand-p ()
  "Don't expand after \"_\" or in strings/comments."
  (nvp-abbrev-expand-not-after-punct-p '(?_)))

;;; GDB
(with-eval-after-load 'nvp-repl
  (nvp-repl-add '(c-mode c-ts-mode c++-mode c++-ts-mode)
    :name 'gdb
    :modes '(gud-mode)
    :find-fn (lambda () (ignore-errors (get-buffer gud-comint-buffer)))
    :init #'gdb
    :init-use-hook t))

;; -------------------------------------------------------------------
;;; Snippet helpers

(defsubst nvp-c-header-file-p ()
  (string-match-p "h[xp]*" (nvp:ext)))

;; split string STR on commas, but only when not between <..>
;; eg., "std::vector<std::pair<int,int>> i, int j" =>
;;      ("std::vector<std::pair<int,int>> i" "int j")
(defun nvp-c-split-string (str &optional delim)
  (when (not (zerop (length str)))
    (let ((delim (or delim ?\,))
          (bcount 0)                     ; current opening brace count
          (prev 0)                       ; substring starting location
          (trim-p t)                     ; non-nil if skipping beginning blanks
          res)                           ; list of resulting strings
      (cl-loop for c across str
               for i from 0 upto (length str)
               do (pcase c
                    (`?  (and trim-p (cl-incf prev)))
                    (`?< (cl-incf bcount))
                    (`?> (cl-decf bcount))
                    ((pred (equal delim))
                     (when (zerop bcount)
                       (push (substring str prev i) res)
                       (setf prev (1+ i))
                       (setf trim-p t)))
                    (_ (setf trim-p nil))))
      (push (string-trim-right (substring str prev)) res)
      (nreverse res))))

(defun nvp-c-yas-vars (&optional sep str)
  (let ((vars (mapcar #'nvp-yas-var (nvp-c-split-string (or str (yas-text))))))
    (if sep (s-join sep vars) vars)))

(defun nvp-c-yas-types (&optional sep str)
  (let ((types (mapcar (lambda (s) (car (split-string s nil t " ")))
                       (nvp-c-split-string (or str (yas-text))))))
    (if sep (s-join sep types) types)))

(defun nvp-c-function-signatures (&optional file ignore-main ignore-static)
  "Pull out functions signatures from current buffer using ctags."
  (--when-let (nvp-tag-list-decls "c" "fp" file)
    (if (or ignore-main ignore-static)
        (let ((ignore (format "\\_<%s\\_>"
                              (regexp-opt
                               (delq nil `(,(and ignore-main "main")
                                           ,(and ignore-static "static")))
                               'symbols))))
          (cl-remove-if (lambda (s) (string-match-p ignore s)) it))
      it)))

(defun nvp-c-yas-args-docstring (text)
  "Convert functions args to doxygen params."
  (--when-let (nvp-c-split-string text)
    (mapconcat 'identity (--map (concat "\n * @param " it) it) "")))

;; `forward-sexp-function' https://github.com/abo-abo/oremacs/
(defun nvp-c-forward-sexp-function (arg)
  (if (looking-at "^#if")
      (forward-ifdef arg)               ;FIXME: forward after final #endif
    (let ((forward-sexp-function nil))
      (forward-sexp arg)
      (while (looking-at "[.-]")
        (forward-sexp)))
    (when (and (eq (char-after) ?.)
               (looking-back "[0-9]+" (line-beginning-position)))
      (forward-char)
      (skip-chars-forward "0-9"))))

(defun nvp-c-toggle-doxygen ()
  "Toggle doxygen comment."
  (interactive)
  (save-excursion
    (when (re-search-forward "\\(?://\\|/\\*+\\)" nil 'move)
      (if (and (string= (match-string 0) "/**") (eq (char-after) ?<))
          (progn (delete-char -1)
                 (delete-char 1))
        (delete-char -1)
        (insert "**<")
        (end-of-line)
        (unless (looking-back "\\*/\\s-*" (line-beginning-position))
          (delete-horizontal-space)
          (insert " */"))))))

(defun nvp-c-align-doxygen (beg end)
  "Align comment from BEG to END for doxygen region."
  (interactive "*r")
  (let (indent-tabs-mode align-to-tab-stop)
    (align-regexp beg end "\\(\\s-*\\)/\\*\\*")
    (align-regexp beg end "\\(\\s-*\\)\\*/")))

;;; Font-lock
(dolist (mode '(c-mode c++-mode))
  (nvp:font-lock-add-defaults mode
    ("\\<\\(assert\\|DEBUG\\)\\s-*(" (1 font-lock-warning-face prepend))))


;; -------------------------------------------------------------------
;;; Tree-sitter

;; Fontify doc comments and comment delimiters
(defun nvp-c-ts--fontify-comment (node override start end &rest _)
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

(defun nvp-c-ts--fontify-call-expression (node override start end &rest _args)
  (pcase (treesit-node-type node)
    ((or "template_function" "qualified_identifier")
     (nvp-c-ts--fontify-call-expression
      (treesit-node-child-by-field-name node "name") override start end))
    ("field_expression"
     (nvp-c-ts--fontify-call-expression
      (treesit-node-child-by-field-name node "field") override start end))
    ("identifier"
     (treesit-fontify-with-override
      (treesit-node-start node) (treesit-node-end node)
      'font-lock-function-call-face
      override start end))
    (_ nil)))

;; Most of stdlib.h functions
(defvar nvp-c-ts--builtins
  '("abort" "aligned_alloc" "at_quick_exit" "atof" "atoi" "atol" "bsearch"
    "calloc" "clearenv" "ecvt" "ecvt_r" "exit" "fcvt" "fcvt_r" "free" "gcvt"
    "getenv" "getloadavg" "getsubopt" "malloc" "mblen" "mbstowcs" "mbtowc"
    "memcpy" "memset" "mkdtemp" "mkstemp" "mkstemps" "mktemp" "on_exit"
    "posix_memalign" "putenv" "qecvt" "qecvt_r" "qfcvt" "qfcvt_r" "qgct" "qsort"
    "qsort_r" "quick_exit" "rand" "random" "realloc" "reallocarray" "realpath"
    "rpmatch" "setenv" "srand" "srandom" "system" "unsetenv" "valloc" "wcstombs"
    "wctomb")
  "Builtins to highlight in C.")

;; Add font-locking for SOME_IDENT constants, doc comments, and namespaces in
;; c++.
(defvar c-ts-mode--operators)
(defun nvp-c-ts-font-lock-settings (language)
  (let* ((delims (append ["," ":" ";" "."] (and (eq 'cpp language) '("::"))))
         (ops (append c-ts-mode--operators (and (eq 'cpp language) '("<=>" "..."))))
         (rules (list
                 :language language
                 :feature 'delimiter
                 `([,@delims] @font-lock-delimiter-face
                   (conditional_expression ["?" ":"] @nvp-ternary-operator-face))
                 :language language
                 :feature 'operator
                 `([,@ops] @font-lock-operator-face
                   "!" @font-lock-negation-char-face)
                 :language language
                 :feature 'comment
                 :override t
                 '((comment) @nvp-c-ts--fontify-comment)
                 :language language
                 :feature 'constant
                 '([(true) (false) (null)] @font-lock-constant-face
                   ((identifier) @font-lock-constant-face
                    (:match "\\`[_A-Z][_A-Za-z0-9]*\\'"
                            @font-lock-constant-face)))
                 :language language
                 :feature 'function
                 `((call_expression
                    function: (_) @nvp-c-ts--fontify-call-expression)))))

    (apply
     #'treesit-font-lock-rules
     (append
      (list
        :language language
        :feature 'macro
        `((preproc_function_def
           name: (identifier) @font-lock-function-name-face)
          (call_expression
           function: ((identifier) @font-lock-preprocessor-face
                      (:match "[A-Z_][A-Z0-9_]*$"
                              @font-lock-preprocessor-face)))
          (null) @nvp-nil-face))
       
      (if (eq language 'cpp)
          (list
           :language language
           :feature 'bracket
           '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face
             (template_argument_list ["<" ">"] @font-lock-bracket-face)
             (template_parameter_list ["<" ">"] @font-lock-bracket-face))

           :language language
           :feature 'namespace
           '((using_declaration
              "namespace" (identifier) @nvp-namespace-face)
             (namespace_identifier) @nvp-namespace-use-face)

           :language language
           :feature 'nvp
           '(;; Note(08/11/24): missing declarations from 'definition
             (optional_parameter_declaration
              declarator: (identifier) @font-lock-variable-name-face)
             ;; for (auto x: ...)
             (for_range_loop
              declarator: (identifier) @font-lock-variable-name-face)
             ;; auto& x
             (reference_declarator
              (identifier) @font-lock-variable-name-face)
             ;; auto[&] [x, y]
             (structured_binding_declarator
              _ [(identifier)] @font-lock-variable-name-face)

             ;; <receiver>.func()
             (call_expression
              function: (field_expression
                         argument: (identifier) @nvp-receiver-face)))

           :language language
           :feature 'builtin
           `((call_expression
              function: ((identifier) @font-lock-builtin-face
                         (:match ,(rx bol "__builtin_")
                                 @font-lock-builtin-face)))))
        (list
         :language language
         :feature 'builtin
         `((call_expression
            function: ((identifier) @font-lock-builtin-face
                       (:match
                        ,(rx-to-string
                          `(seq bol (or ,@nvp-c-ts--builtins
                                        (regexp "__builtin_")
                                        (regexp "str[a-z]+"))))
                        @font-lock-builtin-face))))))
      rules))))


;;; Indent

(defvar c-ts-mode-indent-style)

(defun nvp-c-ts--indent-rules (_mode)
  `(((node-is ")") parent-bol 0)
    ((parent-is
      ,(rx (or "argument_list" "parameter_list" "parenthesized_expression"
               "for_range_loop")))
     parent-bol c-ts-mode-indent-offset)
    ;; TODO(08/02/24): patch to align decls
    ;; Note(5/2/24): in `c-ts-mode--indent-styles' (parent-is "declaration")
    ;; is given indent offset of 0?
    ((node-is "init_declarator") prev-sibling 0)
    ((parent-is ,(rx bos "declaration")) parent-bol c-ts-mode-indent-offset)))


(define-advice c-ts-mode--font-lock-settings (:around (orig-fn language) "nvp")
  (append (nvp-c-ts-font-lock-settings language) (funcall orig-fn language)))

(define-advice c-ts-mode--indent-styles (:around (orig-fn mode &rest args) "nvp")
  (let ((indents
         (assoc-default c-ts-mode-indent-style (apply orig-fn mode args))))
    (list (cons c-ts-mode-indent-style
                (append (nvp-c-ts--indent-rules mode) indents)))))

(nvp:treesit-add-rules c++-ts-mode
  :extra-features '(builtin namespace nvp macro))

(nvp:treesit-add-rules c-ts-mode
  :extra-features '(builtin nvp macro))

(provide 'nvp-c)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-c.el ends here
