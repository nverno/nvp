;;; nvp-bash-ts.el --- Bash tree-sitter -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'sh-script)
(nvp:decls)

(nvp:run-once bash-ts-mode (:after (&rest _))
  (dolist (v '(builtin))
    (cl-pushnew v (cadddr treesit-font-lock-feature-list))))

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
  '("alias" "bg" "bind" "break" "builtin" "caller" "cd" "command" "compgen"
    "complete" "compopt" "continue" "coproc" "dirs" "disown" "echo" "enable" "eval"
    "exec" "exit" "fc" "fg" "getopts" "hash" "help" "history" "jobs" "kill" "let"
    "logout" "mapfile" "popd" "printf" "pushd" "pwd" "read" "readarray" "return"
    "set" "shift" "shopt" "source" "suspend" "test" "time" "times" "trap" "type"
    "typeset" "ulimit" "umask" "unalias" "wait"))
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
       '((comment) @font-lock-comment-face)

       :feature 'function
       :language 'bash
       '((function_definition name: (word)
                              ;; Changed to name-face
                              @font-lock-function-name-face))

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
       '(;; FIXME: should fontify variables in substitution
         (command_substitution) @sh-quoted-exec

         (string
          (simple_expansion
           [(variable_name) (special_variable_name)] @font-lock-variable-name-face))

         ;; FIXME: fontify nested expansions recursively
         (string
          (expansion
           (subscript (word) @font-lock-variable-name-face)))

         (string
          (expansion
           (subscript
            index: (simple_expansion [(variable_name)] @font-lock-variable-name-face))))

         (string (expansion operator: _ @font-lock-operator-face))

         (string (expansion (subscript name: (variable_name) @nvp-special-variable-face)))
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
          ((word) @font-lock-builtin-face
           (:match ,(let ((builtins
                           (sh-feature sh-builtins)))
                      (rx-to-string
                       `(seq bol
                             (or ,@builtins)
                             eol)))
                   @font-lock-builtin-face))))

       :feature 'command
       :language 'bash
       `(;; function/non-builtin command calls
         (command_name (word)
                       ;; Changed to call-face
                       @font-lock-function-call-face)
         ;; builtin commands
         ;; (command_name
         ;;  ((word) @font-lock-builtin-face
         ;;   (:match ,(let ((builtins
         ;;                   (sh-feature sh-builtins)))
         ;;              (rx-to-string
         ;;               `(seq bol
         ;;                     (or ,@builtins)
         ;;                     eol)))
         ;;           @font-lock-builtin-face)))
         )

       :feature 'declaration-command
       :language 'bash
       `([,@sh-mode--treesit-declaration-commands] @font-lock-keyword-face)

       :feature 'constant
       :language 'bash
       `((case_item value: [(word)
                            ;; Added
                            (extglob_pattern)]
                    @font-lock-constant-face)
         (file_descriptor) @font-lock-constant-face

         ;; Added
         (file_redirect
          destination: ((word) @font-lock-constant-face
                        (:match "/dev/null" @font-lock-constant-face)))

         ((word) @font-lock-constant-face
          (:match ,(rx bol (or "true" "false") eol)
                  @font-lock-constant-face)))

       :feature 'operator
       :language 'bash
       `(;; Added
         (case_item "|" @font-lock-delimiter-face)

         [,@sh-mode--treesit-operators
          ;; Added
          (test_operator)]
         @font-lock-operator-face

         ;; Added
         (ternary_expression ["?" ":"] @font-lock-operator-face)

         (expansion operator: _ @font-lock-operator-face)

         (binary_expression
          operator: _ @font-lock-operator-face)

         (postfix_expression
          operator: _ @font-lock-operator-face))

       :feature 'builtin-variable
       :language 'bash
       `(((special_variable_name) @font-lock-builtin-face
          (:match ,(let ((builtin-vars (sh-feature sh-variables)))
                     (rx-to-string
                      `(seq bol
                            (or ,@builtin-vars)
                            eol)))
                  @font-lock-builtin-face))

         ;; Added
         (special_variable_name) @font-lock-variable-name-face

         (expansion
          (subscript
           index: ((word) @font-lock-variable-name-face
                   (:match ,(rx bol (or "@" "*") eol)
                           @font-lock-variable-name-face))))

         ((word) @font-lock-builtin-face
          (:match ,(rx-to-string
                    `(seq bol (or ,@sh-mode--treesit-builtin-constants) eol))
                  @font-lock-builtin-face)))

       :feature 'number
       :language 'bash
       `(((word) @font-lock-number-face
          (:match "\\`[0-9]+\\'" @font-lock-number-face))
         ;; Added: previous rule isnt relevant
         (number) @font-lock-number-face)

       :feature 'bracket
       :language 'bash
       '((["(" ")" "((" "))" "[" "]" "[[" "]]" "{" "}"
           ;; Added
           "$((" "<(" "$("])
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
         ["``"] @font-lock-misc-punctuation-face)))

(provide 'nvp-bash-ts)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-bash-ts.el ends here
