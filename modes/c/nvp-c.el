;;; nvp-c.el --- c helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; TODO:
;; - merge all the compile stuff and remove compile macros
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-compile))
(require 'nvp)
(require 'nvp-parse)
(nvp:req 'nvp-c 'subrs)
(nvp:decls :f (forward-ifdef clang-complete-load-args asdf-where nvp-env-add nvp-yas-var
                             s-join objdump-mode)
           :v (c/R-abbrev-table company-clang-arguments gud-comint-buffer))
(nvp:auto "nvp-tag" 'nvp-tag-list-decls)

(defvar-local nvp-c-local-include-paths '("." ".." "../include"))

;; newline-dwim
(nvp:defmethod nvp-newline-dwim-comment (syntax arg)
  :modes (c-mode c-ts-mode)
  (nvp-newline-dwim--comment syntax arg " * "))

;; #<marker at 78109 in cc-cmds.el.gz>
(cl-defmethod nvp-parse-current-function (&context (major-mode c-mode) &rest _args)
  (add-log-current-defun))

;;; Font-lock
(dolist (mode '(c-mode c++-mode))
  (nvp:font-lock-add-defaults mode
    ("\\<\\(assert\\|DEBUG\\)\\s-*(" (1 font-lock-warning-face prepend))))

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
;; don't expand after '_' or in strings/comments
(defun nvp-c-abbrev-expand-p ()
  (nvp-abbrev-expand-not-after-punct-p '(_)))

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

(defun nvp-header-file-p ()
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

;; pull out functions signatures from current buffer using ctags
(defun nvp-c-function-signatures (&optional file ignore-main ignore-static)
  (--when-let (nvp-tag-list-decls "c" "fp" file)
    (if (or ignore-main ignore-static)
        (let ((ignore (regexp-opt
                       (cl-remove-if
                        #'null
                        (list (and ignore-main "main")
                              (and ignore-static "static")))
                       'symbols)))
          (cl-remove-if (lambda (s) (string-match-p ignore s)) it))
      it)))

;; convert functions args to doxygen params
(defun c-yas-args-docstring (text)
  (let ((args (nvp-c-split-string text)))
    (and args
         (mapconcat 'identity
                    (mapcar (lambda (s) (concat "\n * @param " s)) args) ""))))

;;; Company
(defun nvp-c-load-company-backend (backend)
  (interactive
   (list
    (eval
     (cadr
      (read-multiple-choice
       "Load: "
       '((?c 'company-clang)
         (?i 'company-irony)))))))
  (nvp-company-local backend)
  (setq company-clang-arguments (clang-complete-load-args)))


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


;; -------------------------------------------------------------------
;;; Compile

(defvar nvp-c-address-error-regexp
  '(address-sanitizer
    "^\\s-*#[0-9]+ [x[:xdigit:]]+ in [^\n]* \\([^\n:]+\\):\\([0-9]+\\)" 1 2)
  "Compilation error regexp entry to match address sanitizer stack traces.")

(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist 'address-sanitizer t)
  (add-to-list 'compilation-error-regexp-alist-alist nvp-c-address-error-regexp t))

(defsubst nvp-c--compiler ()
  (if (memq major-mode '(c++-mode c++-ts-mode))
      (nvp:program "g++")
    (nvp:program "gcc")))

;; run make / cmake if there are corresponding makefiles,
;; otherwise prompt / use default
(nvp-make-or-compile-fn nvp-c-compile
  (:default-prompt (read-from-minibuffer "Compiler flags: "))
  (let* ((flags (or args "-Wall -Werror -O2 -ggdb -std=c11"))
         (file (file-name-nondirectory buffer-file-name))
         (out (file-name-sans-extension file))
         (command
          (format "%s %s -o %s%s %s" (nvp-c--compiler)
                  flags out (nvp:with-gnu/w32 ".out" ".exe") file)))
    (unless (or args (assoc 'compile-command (buffer-local-variables)))
      (setq-local compile-command command))
    (funcall-interactively 'nvp-compile current-prefix-arg)))

;; compile current file and run it with output to compilation buffer
(defun nvp-c-compile-and-run (keep &optional compiler flags post-action)
  (interactive "P")
  (let* ((out (concat (file-name-sans-extension
                       (file-name-nondirectory buffer-file-name))
                      (nvp:with-gnu/w32 ".out" ".exe")))
         (command
          (concat (or compiler (nvp-c--compiler)) " "
                  (or flags "-s -O3") " "
                  buffer-file-name " -o " out "; "
                  (or (and (eq post-action 'no-run) "")
                      post-action (concat "./" out))
                  (unless keep (concat "; rm " out)))))
    (setq-local compile-command command)
    (funcall-interactively 'nvp-compile current-prefix-arg)))

;; watch error output with TEST
(defun nvp-c-compile-watch (arg)
  (interactive "P")
  (let ((file (if arg (read-from-minibuffer "Output file: " "out.txt")
                "out.txt"))
        (out (file-name-nondirectory (nvp:c-out-file))))
    (nvp-c-compile-and-run
     nil nil "-O3 -DTEST -std=c11"
     (concat "./" out " 2> " file
             "& gnome-terminal -x watch tail -n10 " file))))

(defun nvp-c-compile-debug ()
  (interactive)
  (nvp-c-compile-and-run 'keep nil "-Wall -Werror -ggdb3 -DDEBUG" 'no-run)
  (call-interactively 'gdb))

;; show assembly in other window, delete assembly output
(defun nvp-c-compile-asm ()
  (interactive)
  (let ((compile-command (format "%s -Og -S %s" (nvp-c--compiler) buffer-file-name))
        (asm-file
         (concat (file-name-sans-extension buffer-file-name) ".s")))
    (with-current-buffer (call-interactively 'nvp-compile)
      (pop-to-buffer (current-buffer))
      (add-hook 'compilation-finish-functions
                (lambda (_b _s)
                  (find-file-other-window asm-file)
                  (add-hook 'kill-buffer-hook
                            (lambda () (delete-file buffer-file-name))
                            nil 'local))
                nil 'local))))

;; dump objects in compilation buffer, setup imenu for function jumps
;; FIXME: add tab/backtab movement
;; (autoload 'gdb-disassembly-mode "gdb-mi")
(defun nvp-c-compile-objdump ()
  (interactive)
  (let ((compile-command (format "%s -Og -c %s; objdump -d %s.o; rm %s.o"
                                 (nvp-c--compiler)
                                 buffer-file-name
                                 (file-name-sans-extension buffer-file-name)
                                 (file-name-sans-extension buffer-file-name)))
        compilation-scroll-output)
    (with-current-buffer (call-interactively 'nvp-compile)
      (pop-to-buffer (current-buffer))
      ;; (gdb-disassembly-mode)
      (objdump-mode)
      (setq-local imenu-generic-expression '((nil "^[0-9]+ <\\([^>]+\\)>:" 1)))
      (add-hook 'compilation-finish-functions
                (lambda (_b _s)
                  (search-forward "Disassembly" nil 'move 1))
                nil 'local))))

(defun nvp-c-compile-strace (&optional arg)
  (interactive "P")
  (let* ((prog (file-name-sans-extension buffer-file-name))
         (strace-file (concat prog ".strace"))
         (compile-command
          (format "%s -Og %s -o %s; strace -o %s %s %s"
                  (nvp-c--compiler) buffer-file-name prog strace-file prog
                  (if arg (read-from-minibuffer
                           "Additional arguments to function: ")
                    "")))
         compilation-scroll-output)
    (with-current-buffer (call-interactively 'nvp-compile)
      (pop-to-buffer (current-buffer))
      (add-hook 'compilation-finish-functions
                (lambda (_b _s)
                  (find-file-other-window strace-file)
                  (add-hook 'kill-buffer-hook
                            (lambda () (delete-file buffer-file-name))
                            nil 'local))
                nil 'local))))
  
;;; XREFs

;;;###autoload
(define-advice semantic-ia-fast-jump (:around (orig-fn &rest args) "push-mark")
  (xref-push-marker-stack)
  (condition-case nil
      (apply orig-fn args)
    (error (xref-go-back))))

;;; Environment

(defvar nvp-c-ext-includes
  '(("unity" (expand-file-name ".local/include/unity/src" (getenv "HOME"))
     "/unity/src")
    ("R"     (expand-file-name "lib/R/include" (asdf-where "R")) "/R/include")
    ("emacs" emacs-src-dir "/emacs/src"))
  "Paths to external includes.")

;; set environment stuff for macro expanding
;; could also set local `c-macro-preprocessor'?
(defun nvp-c-setenv (type)
  "Add include path of TYPE to macroexpand stuff."
  (interactive
   (list (ido-completing-read "Add include path for: " nvp-c-ext-includes)))
  (cl-destructuring-bind (kind loc regex) (assoc-string type nvp-c-ext-includes)
    (pcase kind
      (`"R"
       (setq-local local-abbrev-table c/R-abbrev-table)
       (setq-local nvp-abbrev-local-table "c/R"))
      (_ nil))
    (nvp-env-add "C_INCLUDE_PATH" (eval loc) regex)))

;; -------------------------------------------------------------------
;;; Doxygen

(defun nvp-c-toggle-doxygen ()
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

;; -------------------------------------------------------------------
;;; Align/tidy

;; align comment start / end for doxygen region
(defun nvp-c-align-doxygen (beg end)
  (interactive "*r")
  (let (indent-tabs-mode align-to-tab-stop)
    (align-regexp beg end "\\(\\s-*\\)/\\*\\*")
    (align-regexp beg end "\\(\\s-*\\)\\*/")))

(provide 'nvp-c)
;;; nvp-c.el ends here
