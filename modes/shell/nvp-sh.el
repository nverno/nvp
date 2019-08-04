;;; nvp-sh.el --- sh script helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; TODO:
;; sh-comp:
;;  - create docstrings from function headers

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-shell-macs "macs/nvp-shell-macs")
  (require 'nvp-sh-help)               ;defsubsts
  (require 'nvp-font)
  (require 'nvp-parse))
(require 'nvp)
(require 'company)
(require 'company-quickhelp)
(require 'sh-script)
(require 'nvp-shell-common)
(nvp-decls)
(nvp-decl company-shell sh-comp-candidates sh-comp--xref-backend
  sh-comp-completion-at-point)
(nvp-auto "nvp-sh-help" nvp-sh-help-bash-builtin-p nvp-sh-help-bash-builtin-sync)

;; -------------------------------------------------------------------
;;; Variables

;; for jumping b/w functions -- see `sh-imenu-generic-expression'
(defvar nvp-sh-function-re
  (nvp-concat
   "\\(?:"
   ;; function FOO()
   "^\\s-*function\\s-+\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*\\(?:()\\)?"
   "\\|"
   ;; FOO()
   "^\\s-*\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*()"
   "\\)"))

;; imenu header comment regexp
(defvar nvp-sh-comment-headers-re '((nil "^###\\s-*\\(.+\\)\\s-*$" 1)))

;; additional imenu regexps
(defvar nvp-sh-imenu-extra-regexps
  '(("Sources" "^\\(?:\\\.\\|source\\)\\s-+\\(.+\\)\\s-*$" 1)
    ("Globals" "^\\([[:alpha:]_][[:alnum:]_]*\\)=" 1)))

;;; Font-locks

;; Add font-locking & register additions
(nvp-font-lock-add-defaults 'sh-mode
  ;; gaudy array faces
  ("\\${\\([!#?]?[[:alpha:]_][[:alnum:]_]*\\[[@*]\\]\\)}"
   (1 'nvp-italic-variable-face prepend))
  ;; redirections
  ("\\([0-9&<>]*[ ]*/dev/null\\)" (1 'nvp-italic-type-face prepend))
  ;; quoted vars, special vars, function arguments
  (:quoted ?\" "\\${?\\([[:alpha:]_][[:alnum:]_]*\\|[-#?@!*]\\|[0-9]\\)"
           (1 font-lock-variable-name-face prepend))
  ;; first function in quoted backquote expressions, "`cmd ...`"
  (:quoted ?\" "`\\s-*\\([[:alnum:]_\\-]+\\)" (1 'sh-quoted-exec prepend)))

;;; Toggle

;; toggle variable default assignment
(defun nvp-sh-toggle-variable ()
  (interactive)
  (when (re-search-backward
         "\\(\\_<[[:alnum:]_]+\\)=.*" (line-beginning-position) t)
    (let* ((var (concat "${" (match-string 1) ":-")))
      (goto-char (match-end 1))
      (skip-chars-forward "=\"")
      (if (looking-at (regexp-quote var))
          (progn
            (save-match-data
              (skip-syntax-forward "'")
              (forward-sexp 1)
              (and (eq (char-before) ?})
                   (delete-char -1)))
            (replace-match ""))
        (insert var)
        (forward-sexp 1)
        (insert "}")))))


;; -------------------------------------------------------------------
;;; Generics

(eval-when-compile
  (defmacro nvp-sh:candidates (type args)
    `(nvp-parse:buffer-file nil nil ,args
       (sh-comp-candidates ',type file))))

(cl-defmethod nvp-parse-current-function (&context (major-mode sh-mode) &rest _args)
  "Find name of function containing point.
Like `sh-current-defun-name' but ignore variables."
  (save-excursion
    (end-of-line)
    (when (re-search-backward nvp-sh-function-re nil 'move)
      (or (match-string-no-properties 1)
          (match-string-no-properties 2)))))

(cl-defmethod nvp-parse-functions (&context (major-mode sh-mode) &rest args)
  "Functions available in buffer/file, including sources."
  (nvp-sh:candidates functions args))

(cl-defmethod nvp-parse-variables (&context (major-mode sh-mode) &rest args)
  "Global variables available in file including sources."
  (nvp-sh:candidates variables args))

(cl-defmethod nvp-parse-includes (&context (major-mode sh-mode) &rest args)
  "Sourced files, recursively."
  (nvp-sh:candidates sources args))


;;; Navigation
;; commands to enable `beginning-of-defun', `end-of-defun', `narrow-to-defun',
;; etc. to work properly in sh buffers

(defsubst nvp-sh-looking-at-beginning-of-defun ()
  (save-excursion
    (beginning-of-line 1)
    (looking-at-p nvp-sh-function-re)))

(defun nvp-sh--beginning-of-defun (&optional arg)
  "Internal implementation for function navigation.
With positive ARG search backwards."
  (when (or (null arg) (= arg 0)) (setq arg 1))
  (let ((search-fn (if (> arg 0)
                       #'re-search-backward
                     #'re-search-forward))
        (pos (point-marker)))
    (and (< arg 0)                          ;searching forward -- skip current func
         (nvp-sh-looking-at-beginning-of-defun)
         (end-of-line 1))
    (funcall search-fn nvp-sh-function-re nil 'move)
    (if (nvp-sh-looking-at-beginning-of-defun)
        (or (beginning-of-line 1) (point))  ;found function
      (and (goto-char pos) nil))))          ;failed to find one

(defun nvp-sh-beginning-of-defun (&optional arg)
  "Move to beginning of defun.
With positive ARG search backwards, otherwise forwards.
Used to set `beginning-of-defun-function'."
  (when (or (null arg) (= arg 0)) (setq arg 1))
  (let (found)
    (while (and (not (= arg 0))
                (let ((keep-searching-p (nvp-sh--beginning-of-defun arg)))
                  (when (and keep-searching-p (null found))
                    (setq found t))
                  keep-searching-p))
      (setq arg (if (> arg 0) (1- arg) (1+ arg))))
    found))

(defun nvp-sh-end-of-defun ()
  "Move point to end of current function.
Used to set `end-of-defun-function'."
  (when (or (nvp-sh-looking-at-beginning-of-defun) ;find beginning
            (nvp-sh-beginning-of-defun 1)
            (nvp-sh-beginning-of-defun -1))
    (beginning-of-line)
    (when (search-forward "{" nil 'move) ;move to opening '{' and jump sexp
      (forward-char -1)
      (forward-list)
      (point))))


;;; Company / Help

;; FIXME: generalize toggle to call indirectly
(defvar nvp-sh-company-active-map
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km company-active-map)
    (define-key km [remap nvp-company-quickhelp-toggle] #'nvp-sh-quickhelp-toggle)
    (define-key km [remap company-show-doc-buffer] #'nvp-sh-company-show-doc-buffer)
    km))

;; with prefix, only complete for sourced / local functions
(defun nvp-sh-company-complete (arg)
  "Temporarily use only sourced / local functions for completion."
  (interactive "P")
  (if arg (call-interactively 'company-sh-comp)
    (company-complete)))

;; Since no doc-buffer is returned by company-capf, rewrite
;; company-quickhelp doc retrieval method to just call man on the
;; current candidates
;; return a company-documentation buffer with either Man output or bash help
;; for a builtin
;; FIXME: nvp-sh-help is being loaded just for these functions
(defun nvp-sh-doc-buffer (cmd)
  (let ((doc-str (if (nvp-sh-help-bash-builtin-p cmd)
                     (nvp-sh-help-bash-builtin-sync cmd)
                   (shell-command-to-string (format "man %s" cmd)))))
    (and (not (or (member doc-str '(nil ""))
                  (string-prefix-p "No manual entry" doc-str)))
         (company-doc-buffer doc-str))))

;; FIXME: move formatting to `nvp-doc' and generalize
(defun nvp-sh-quickhelp-doc (selected)
  (cl-letf (((symbol-function 'completing-read)
             #'company-quickhelp--completing-read))
    (let* ((doc-buff (nvp-sh-doc-buffer selected))
           (doc-and-meta
            (with-current-buffer doc-buff
              (company-quickhelp--docstring-from-buffer (point-min))))
           (truncated (plist-get doc-and-meta :truncated))
           (doc (plist-get doc-and-meta :doc)))
      (unless (member doc '(nil ""))
        (if truncated
            (concat doc "\n\n[...]")
          doc)))))

;; FIXME: use `nvp-company-quickhelp-toggle', fixes bugs and can call indirect func
;; re-bind to this in `company-active-map'
(defun nvp-sh-quickhelp-toggle ()
  (interactive)
  (let ((x-gtk-use-system-tooltips nil)
        ;; (company-quickhelp-delay 0.1)
        )
    (or (x-hide-tip)
        (cl-letf (((symbol-function 'company-quickhelp--doc) #'nvp-sh-quickhelp-doc))
          ;; flickers the screen - cant use the timer, since it seems
          ;; that lexical binding doesn't work in that case
          (company-quickhelp--show)))))

;; FIXME: use `nvp-doc' generic method to show docs
;; show help buffer in other window from company-active-map
(defun nvp-sh-company-show-doc-buffer ()
  (interactive)
  (cl-letf (((symbol-function 'company-call-backend)
             #'(lambda (_type selected)
                 (nvp-sh-doc-buffer selected) "*company-documentation*")))
    (company-show-doc-buffer)))


;;; REPL

;; FIXME: replace with nvp-proc function
;; replacement for `sh-shell-process'
(defun nvp-sh-get-process (&optional this-buffer)
  "Replacement for `sh-shell-process'.
Optionally return process specific to THIS-BUFFER."
  (let ((buffname (and this-buffer
                       (concat "*shell: " (buffer-name (current-buffer)) "*"))))
    (if (and (process-live-p sh-shell-process)
             (if buffname
                 (string= (buffer-name
                           (process-buffer sh-shell-process))
                          buffname)
               t))
        sh-shell-process
      (setq sh-shell-process
            (let ((proc (nvp-shell-get-process nil buffname)))
              (or proc
                  (get-buffer-process
                   (let ((explicit-shell-file-name sh-shell-file))
                     (shell buffname)))))))))

;; FIXME: do I always want a sh file to sending to its own shell?
(setf (symbol-function 'sh-shell-process) 'nvp-sh-get-process)

;; FIXME: remove, this is covered by generic REPL interface
;; send selected region and step
(defun nvp-sh-send-region (beg end)
  "Send selected region from BEG to END to associated shell process."
  (interactive "r")
  (comint-send-string (nvp-sh-get-process)
                      (concat (buffer-substring beg end) "\n"))
  (goto-char end))


;;; Cleanup / Align

(require 'align)

;; FIXME: these rules can be useful for other modes: makefile, automake, etc.
;; alignment rules to align '\' not in strings/comments and
;; align end-of-line comments
(defvar nvp-sh-align-rules-list
  `((sh-line-continuation
     (regexp . "\\(\\s-*\\)\\\\\\s-*$")
     (modes  . '(sh-mode))
     (valid  . ,(function
                 (lambda () (save-excursion
                         (not (sh-in-comment-or-string (point))))))))
    (sh-eol-comments
     (regexp . "[^ #\t\n\\\\]\\(\\s-+\\)#+.*$")
     (group  . 1)
     (modes  . '(sh-mode))
     (valid  . ,(function
                 (lambda () (save-excursion
                         (goto-char (match-beginning 1))
                         (and (not (bolp))
                              (not (nth 3 (syntax-ppss)))))))))))


;;; Hooks

;; enforce uft-8-unix and align when killing buffer
(defun nvp-sh-tidy-buffer ()
  (unless (or buffer-read-only (not (buffer-modified-p)))
    (ignore-errors (align (point-min) (point-max)))
    (and (buffer-modified-p)
         (save-buffer))))

(defun nvp-sh-locals ()
  "Define local variables to be called in hook."
  (setq-local beginning-of-defun-function #'nvp-sh-beginning-of-defun)
  (setq-local end-of-defun-function #'nvp-sh-end-of-defun)
  (setq-local align-rules-list nvp-sh-align-rules-list)
  (make-local-variable 'hippie-expand-try-functions-list)
  (cl-pushnew 'nvp-he-try-expand-shell-alias hippie-expand-try-functions-list)
  (add-hook 'kill-buffer-hook #'nvp-sh-tidy-buffer nil 'local)
  (add-hook 'xref-backend-functions #'sh-comp--xref-backend nil t)
  ;; Completion
  (add-hook 'completion-at-point-functions #'sh-comp-completion-at-point nil t)
  ;; local version of `company-active-map' to rebind popup/help functions
  (setq-local company-active-map nvp-sh-company-active-map)
  (setq-local company-backends (remq 'company-capf company-backends))
  (push 'company-capf company-backends)
  (setq-local company-transformers '(company-sort-by-backend-importance))
  (unless (require 'bash-completion nil t)
    ;; only use company-shell if bash-completion isn't installed
    (push 'company-shell company-backends)))

(provide 'nvp-sh)
;;; nvp-sh.el ends here
