;;; nvp-bash-ts.el --- Bash tree-sitter -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'sh-script)
(nvp:decls)

(defface bash-file-redirect-face
  '((t (:inherit font-lock-type-face :weight bold :slant italic)))
  "Face for bash special file redirects."
  :group 'bash)

(defface bash-file-descriptor-number-face
  '((t (:inherit font-lock-number-face :weight bold)))
  "Face for bash numeric file descriptors."
  :group 'bash)

(defface bash-special-variable-face
  '((t (:inherit font-lock-escape-face)))
  "Face for bash special variables."
  :group 'bash)

(defface bash-expansion-variable-face
  '((t (:inherit font-lock-variable-name-face :slant italic)))
  "Face for bash expansion variable."
  :group 'bash)

(nvp:treesit-add-rules bash-ts-mode
  :extra-features '(builtin property escape-sequence))


;;; Font-locking

(defun bash-ts--fontify-bracket-operator (node override start end &rest _)
  (let ((node-start (treesit-node-start node))
        (node-end (treesit-node-end node)))
    (treesit-fontify-with-override
     node-start (1+ node-start) 'font-lock-operator-face override start end)
    (treesit-fontify-with-override
     (1+ node-start) node-end 'font-lock-bracket-face override start end)))

(defun bash-ts--fontify-alias (node override start end &rest _)
  (let ((node-start (treesit-node-start node))
        (node-end (treesit-node-end node)))
    (treesit-fontify-with-override
     node-start (1- node-end) 'font-lock-variable-name-face override start end)
    (treesit-fontify-with-override
     (1- node-end) node-end 'font-lock-operator-face override start end)))

(defconst bash-ts--doc-comment-re
  (rx bol
      (group (+ "#")) (* white)
      (group "@" (* alnum)) (* white)
      (or (seq "[" (group-n 3 (* (not "]"))) "]")
          (seq "<" (group-n 3 (* (not ">"))) ">")
          (group-n 3 (+ (not space))))
      (* white)
      (group (* nonl)))
  "Regex to match bash doc comments.")

(defun bash-ts--fontify-doc-comment (node override start end &rest _)
  (save-excursion
    (goto-char (treesit-node-start node))
    (save-match-data
      (when (looking-at bash-ts--doc-comment-re)
        (treesit-fontify-with-override
         (match-beginning 1) (match-end 1)
         'font-lock-comment-delimiter-face override start end)
        (treesit-fontify-with-override
         (match-beginning 2) (match-end 2)
         'font-lock-constant-face override start end)
        (treesit-fontify-with-override
         (match-beginning 3) (match-end 3)
         (if (equal "@see" (match-string 2))
             'link
           'font-lock-variable-name-face)
         override start end)
        (treesit-fontify-with-override
         (match-beginning 4) (match-end 4)
         'font-lock-comment-face override start end)))))

(defun bash-ts--fontify-shellcheck-comment (node override start end &rest _)
  (save-excursion
    (goto-char (treesit-node-start node))
    (when (looking-at "\\(#+\\)[ \t]*\\(shellcheck .*\\)")
      (treesit-fontify-with-override
         (match-beginning 1) (match-end 1)
         'font-lock-comment-delimiter-face override start end)
        (treesit-fontify-with-override
         (match-beginning 2) (match-end 2)
         '(:inherit font-lock-comment-face :slant italic) override start end))))

(setq sh-mode--treesit-operators
      '("|" "|&" "||" "&&" ">" ">>" "<" "<<" "<<-" "<<<" "==" "!=" ";&" ";;&"
        ;; Added
        "=" "+=" "=~" "&>" "&>>" "<&" ">&" ">|" "<&-" ">&-" ".." "!"))

;;; Added
;; trap -l
(defvar sh-mode--treesit-builtin-constants
  '("SIGHUP" "SIGINT" "SIGQUIT" "SIGILL" "SIGTRAP" "SIGABRT" "SIGBUS" "SIGFPE"
    "SIGKILL" "SIGUSR1" "SIGSEGV" "SIGUSR2" "SIGPIPE" "SIGALRM" "SIGTERM"
    "SIGSTKFLT" "SIGCHLD" "SIGCONT" "SIGSTOP" "SIGTSTP" "SIGTTIN" "SIGTTOU"
    "SIGURG" "SIGXCPU" "SIGXFSZ" "SIGVTALRM" "SIGPROF" "SIGWINCH" "SIGIO"
    "SIGPWR" "SIGSYS" "SIGRTMIN" "SIGRTMIN+1" "SIGRTMIN+2" "SIGRTMIN+3"
    "SIGRTMIN+4" "SIGRTMIN+5" "SIGRTMIN+6" "SIGRTMIN+7" "SIGRTMIN+8"
    "SIGRTMIN+9" "SIGRTMIN+10" "SIGRTMIN+11" "SIGRTMIN+12" "SIGRTMIN+13"
    "SIGRTMIN+14" "SIGRTMIN+15" "SIGRTMAX-14" "SIGRTMAX-13" "SIGRTMAX-12"
    "SIGRTMAX-11" "SIGRTMAX-10" "SIGRTMAX-9" "SIGRTMAX-8" "SIGRTMAX-7"
    "SIGRTMAX-6" "SIGRTMAX-5" "SIGRTMAX-4" "SIGRTMAX-3" "SIGRTMAX-2"
    "SIGRTMAX-1" "SIGRTMAX"))

;;; Added
(defvar sh-mode--treesit-builtin-functions
  (seq-uniq
   (append
    '("alias" "bg" "bind" "break" "builtin" "caller" "cd" "command" "compgen"
      "complete" "compopt" "continue" "coproc" "dirs" "disown" "echo" "enable" "eval"
      "exec" "exit" "fc" "fg" "getopts" "hash" "help" "history" "jobs" "kill" "let"
      "logout" "mapfile" "popd" "printf" "pushd" "pwd" "read" "readarray" "return"
      "set" "shift" "shopt" "source" "suspend" "test" "time" "times" "trap" "type"
      "typeset" "ulimit" "umask" "unalias" "wait"
      ;; Not actually builtins
      "sudo")
    (sh-feature sh-builtins))))

;;; Note: the following are missing from (sh-feature sh-builtins)
;; '("break" "compopt" "continue" "exec" "exit" "logout" "return" "time" "trap")

(setq sh-mode--treesit-keywords
      '("case" "do" "done" "elif" "else" "esac" "export" "fi" "for"
        "function" "if" "in" "unset" "while" "then"
        ;; Added
        "select" "until"
        "declare" "typeset" "readonly" "local" "unsetenv"))


(setq sh-mode--treesit-settings
      (treesit-font-lock-rules
       :feature 'comment
       :language 'bash
       `(((comment) @bash-ts--fontify-doc-comment
          (:match ,bash-ts--doc-comment-re @bash-ts--fontify-doc-comment))
         ((comment) @bash-ts--fontify-shellcheck-comment
          (:match ,(rx bol (+ "#") (* white) "shellcheck")
                  @bash-ts--fontify-shellcheck-comment))
         (comment) @font-lock-comment-face

         (program
          :anchor ((comment) @nvp-treesit-fontify-hash-bang
                   (:match ,(rx bol "#!") @nvp-treesit-fontify-hash-bang))))

       :feature 'function
       :language 'bash
       `((function_definition
          name: (word)
          ;; Changed to name-face
          @font-lock-function-name-face)

         ;; Fontify alias assignments
         (command
          name: ((command_name) @_name
                 (:match ,(rx bol "alias" eol) @_name))
          argument: (concatenation
                     ((word) @bash-ts--fontify-alias
                      (:match ,(rx "=" eol) @bash-ts--fontify-alias)))))

       :feature 'string
       :language 'bash
       '([(string) (raw_string)
          ;; Added
          (ansi_c_string)]
         @font-lock-string-face
         ;; Added
         ;; (concatenation
         ;;  (word) @font-lock-string-face)
         [(regex) ;; (extglob_pattern)
          ]
         @font-lock-regexp-face)

       :feature 'string-interpolation
       :language 'bash
       :override t
       ;; FIXME: remove redundant rules
       `((command_substitution) @sh-quoted-exec

         (special_variable_name) @bash-special-variable-face

         (subscript
          index: (word) @font-lock-variable-use-face)

         (subscript
          index: ((word) @bash-special-variable-face
                  (:match ,(rx bol (or "*" "@") eol)
                          @bash-special-variable-face)))
         (subscript
          index: (number) @font-lock-number-face)

         (string
          (simple_expansion
           (variable_name) @font-lock-variable-name-face))

         (string
          (expansion
           (subscript (word) @font-lock-variable-name-face)))

         (string
          (expansion
           (subscript
            index: (simple_expansion [(variable_name)] @font-lock-variable-name-face))))

         (string (expansion operator: _ @font-lock-operator-face))

         (expansion
          (subscript
           name: (variable_name) @bash-expansion-variable-face))
         ;; (string (expansion ["${" "}"] @font-lock-bracket-face))

         (string (expansion (variable_name) @font-lock-variable-name-face)))
       ;; (string
       ;;  (expansion
       ;;   _;"${" @font-lock-escape-face
       ;;   operator: "!" @font-lock-escape-face :?
       ;;   (subscript
       ;;    name: (variable_name) @nvp-special-variable-face
       ;;    index: (word) @nvp-special-variable-face :?)
       ;;   ;; name: (variable_name)
       ;;   ;; index: (word) @nvp-special-variable-face
       ;;   ;"}" @font-lock-escape-face
       ;;   ))

       :feature 'heredoc
       :language 'bash
       :override 'keep
       '([(heredoc_start) (heredoc_body)
          ;; Added
          (heredoc_end)]
         @sh-heredoc)

       :feature 'variable
       :language 'bash
       '((variable_name) @font-lock-variable-name-face)
       
       :feature 'keyword
       :language 'bash
       `(;; keywords
         [ ,@sh-mode--treesit-keywords ] @font-lock-keyword-face
         ;; reserved words
         (command_name
          ((word) @font-lock-keyword-face
           (:match
            ,(rx-to-string
              `(seq bol
                    (or ,@(sh-mode--treesit-other-keywords))
                    eol))
            @font-lock-keyword-face))))

       ;; Added
       :feature 'builtin
       :language 'bash
       `((command_name
          ((word) @font-lock-preprocessor-face
           (:match ,(rx bol (or "source" "alias" "shopt" "set" "eval") eol)
                   @font-lock-preprocessor-face)))

         (command_name
          ((word) @font-lock-builtin-face
           (:match ,(rx-to-string
                     `(seq bol
                           (or ,@sh-mode--treesit-builtin-functions)
                           eol))
                   @font-lock-builtin-face))))

       :feature 'declaration-command
       :language 'bash
       `([,@sh-mode--treesit-declaration-commands] @font-lock-keyword-face)

       :feature 'constant
       :language 'bash
       `((case_item
          value: [(word) (extglob_pattern)]
          @font-lock-constant-face)

         ;; Added
         (file_redirect
          destination: ((word) @bash-file-redirect-face
                        (:match "/dev/null" @bash-file-redirect-face)))

         (file_descriptor) @bash-file-descriptor-number-face

         (file_redirect
          destination: (number) @bash-file-descriptor-number-face)

         ((word) @font-lock-constant-face
          (:match ,(rx bol (or "true" "false") eol)
                  @font-lock-constant-face)))

       :feature 'command
       :language 'bash
       `(;; function/non-builtin command calls
         ;; Changed to call-face
         (command_name (word) @font-lock-function-call-face))

       :feature 'operator
       :language 'bash
       `(;; Added
         (case_item "|" @font-lock-delimiter-face)

         ["$((" "<(" "$("] @bash-ts--fontify-bracket-operator

         [,@sh-mode--treesit-operators
          ;; Added
          (test_operator)]
         @font-lock-operator-face

         ;; Added(08/02/24)
         ["&"] @font-lock-preprocessor-face

         ;; Added
         (ternary_expression ["?" ":"] @font-lock-operator-face)

         (expansion operator: _ @font-lock-operator-face)

         (binary_expression
          operator: _ @font-lock-operator-face)

         (postfix_expression
          operator: _ @font-lock-operator-face))

       :feature 'builtin-variable
       :language 'bash
       `(;; ((special_variable_name) @bash-special-variable-face
         ;;  (:match ,(rx-to-string
         ;;            `(seq bol
         ;;                  (or ,@(sh-feature sh-variables))
         ;;                  eol))
         ;;          @bash-special-variable-face))

         ;; Added
         ;; (special_variable_name) @bash-special-variable-face

         ;; (expansion
         ;;  (subscript
         ;;   index: ((word) @bash-special-variable-face
         ;;           (:match ,(rx bol (or "@" "*") eol)
         ;;                   @bash-special-variable-face))))

         ((word) @font-lock-builtin-face
          (:match ,(rx-to-string
                    `(seq bol (or ,@sh-mode--treesit-builtin-constants) eol))
                  @font-lock-builtin-face)))

       :feature 'number
       :language 'bash
       `(;; ((word) @font-lock-number-face
         ;;  (:match "\\`[0-9]+\\'" @font-lock-number-face))
         ;; Added: previous rule isnt relevant
         (number) @font-lock-number-face)

       :feature 'bracket
       :language 'bash
       '((["(" ")" "((" "))" "[" "]" "[[" "]]" "{" "}"
           ;; Added
           ;; "$((" "<(" "$("
           ])
         @font-lock-bracket-face)

       :feature 'delimiter
       :language 'bash
       '(([";" ";;"
           ;; Added
           "," ";&" ";;&" "&"])
         @font-lock-delimiter-face)

       :feature 'misc-punctuation
       :language 'bash
       `((["$"]) @font-lock-misc-punctuation-face
         ;; Added
         ["``"] @font-lock-misc-punctuation-face)

       ;; Added(08/02/24)
       :language 'bash
       :feature 'property
       `(((word) @font-lock-property-use-face
          (:match "\\`-[[:alnum:]_-]+" @font-lock-property-use-face)))

       ;; Added(08/02/24)
       :language 'bash
       :feature 'escape-sequence
       `(((word) @font-lock-escape-face
          (:match ,(rx bos "\\" (? (or "!" "(" ")" "[" "]" "\\" ";")))
                  @font-lock-escape-face)))))

(provide 'nvp-bash-ts)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-bash-ts.el ends here
