;;; nvp-sh.el --- sh script helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; TODO:
;; sh-comp:
;;  - create docstrings from function headers
;;
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-parse))
(require 'nvp)
(require 'company)
(require 'sh-script)
(require 'nvp-shell-common)
(nvp:decls :f (sh-comp-candidates))


;;; Company Quickhelp remappings

(nvp:auto "nvp-sh-help" 'nvp-sh-quickhelp-toggle 'nvp-sh-company-show-doc-buffer)
(defvar-keymap nvp-sh-company-active-map
  :parent company-active-map
  "<remap> <company-show-doc-buffer>" #'nvp-sh-company-show-doc-buffer)
;; "<remap> <nvp-company-quickhelp-toggle>" #'nvp-sh-quickhelp-toggle


;;; Imenu
;; For jumping b/w functions -- see `sh-imenu-generic-expression'
(eval-and-compile
  (defconst nvp-sh-function-re
    (concat
     "\\(?:"
     ;; function FOO()
     "^\\s-*function\\s-+\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*\\(?:()\\)?"
     "\\|"
     ;; FOO()
     "^\\s-*\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*()"
     "\\)")))

;; Imenu header comment regexp
(defconst nvp-sh-comment-headers-re '(("Headers" "^###\\s-*\\(.+\\)\\s-*$" 1)))

;; Additional imenu regexps
(defconst nvp-sh-imenu-extra-regexps
  `(("Sources" "^\\(?:\\\.\\|source\\)\\s-+\\(.+\\)\\s-*$" 1)
    ("Globals"
     ,(nvp:concat
       ;; optionally prefixed by: export or declare
       "^\\(?:" (regexp-opt '("export" "declare"))
       ;; with optional flag
       "\\s-*\\(?:-[[:alpha:]]\\)?\\s-*\\)?"
       "\\([[:alpha:]_][[:alnum:]_]*\\)=")
     1)))


;;; Navigation

(defsubst nvp-sh--looking-at-beginning-of-defun ()
  (save-excursion
    (beginning-of-line 1)
    (looking-at-p nvp-sh-function-re)))

(defun nvp-sh--beginning-of-defun (&optional arg)
  "Internal implementation for function navigation.
With positive ARG search backwards."
  (when (or (null arg) (= arg 0)) (setq arg 1))
  (let ((search-fn (if (> arg 0) #'re-search-backward #'re-search-forward))
        (pos (point-marker)))
    (and (< arg 0)                      ; searching forward -- skip current func
         (nvp-sh--looking-at-beginning-of-defun)
         (end-of-line 1))
    (funcall search-fn nvp-sh-function-re nil 'move)
    (if (nvp-sh--looking-at-beginning-of-defun)
        (or (beginning-of-line 1) (point)) ; found function
      (and (goto-char pos) nil))))         ; failed to find one

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
  (when (or (nvp-sh--looking-at-beginning-of-defun) ;find beginning
            (nvp-sh-beginning-of-defun 1)
            (nvp-sh-beginning-of-defun -1))
    (beginning-of-line)
    (when (search-forward "{" nil 'move) ;move to opening '{' and jump sexp
      (forward-char -1)
      ;; Note: will error on malformed sexps
      (forward-list)
      (point))))


;;; Font-locks

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

;; Add font-locking & register additions
(nvp:font-lock-add-defaults 'sh-mode
  ;; Gaudy array faces
  ("\\${\\([!#?]?[[:alpha:]_][[:alnum:]_]*\\[[@*]\\]\\)}"
   (1 'bash-expansion-variable-face prepend))
  ;; Redirections
  ("\\([0-9&<>]*[ ]*/dev/null\\)" (1 'bash-file-redirect-face prepend))
  ;; Doxy params
  ("^# *\\(@[[:alpha:]]+\\)\\s-*\\(\$[[:digit:]]\\)\\(.*\\)$"
   (1 font-lock-constant-face prepend)
   (2 font-lock-variable-name-face prepend))
  ;; Quoted vars, special vars, function arguments
  (:quoted ?\" "\\${?\\([[:alpha:]_][[:alnum:]_]*\\|[-#?@!*]\\|[0-9]\\)"
           (1 font-lock-variable-name-face prepend))
  ;; First function in quoted backquote expressions, "`cmd ...`"
  (:quoted ?\" "`\\s-*\\([[:alnum:]_\\-]+\\)" (1 'sh-quoted-exec prepend)))


;; -------------------------------------------------------------------
;;; REPL

;;; TODO(08/05/24): fix `shell-eval-command' to handle multiline prompt
;;  for eval support

;; Drop-in replacement for `sh-shell-process'
(defun nvp-sh-get-process (&optional this-buffer)
  "Return a shell process specific to THIS-BUFFER if non-nil.
Replacement for `sh-shell-process', which see."
  (let ((bufname (and this-buffer
                      (concat "*shell:" (buffer-name (current-buffer)) "*"))))
    (if (and (process-live-p sh-shell-process)
             (or (null bufname)
                 (string= bufname (buffer-name (process-buffer sh-shell-process)))))
        sh-shell-process                  ; have a live process already
      (setq sh-shell-process
            (let ((proc (nvp-shell-get-process bufname nil sh-shell)))
              (or proc
                  (get-buffer-process
                   (let ((explicit-shell-file-name sh-shell-file))
                     (save-window-excursion
                       (shell bufname))))))))))

;; Overwrite the default function value
(setf (symbol-function 'sh-shell-process) 'nvp-sh-get-process)

;;; Generic Implementations

(cl-defmethod nvp-parse-current-function (&context (major-mode sh-mode) &rest _args)
  "Find name of function containing point.
Like `sh-current-defun-name' but ignore variables."
  (save-excursion
    (end-of-line)
    (when (re-search-backward nvp-sh-function-re nil 'move)
      (or (match-string-no-properties 1)
          (match-string-no-properties 2)))))

(cl-defmethod nvp-parse-functions ((_mode (eql sh-mode)) &rest args)
  "Functions available in buffer/file, including sources."
  (nvp-parse:buffer-file nil nil args
    (sh-comp-candidates 'functions file)))

(cl-defmethod nvp-parse-variables ((_mode (eql sh-mode)) &rest args)
  "Global variables available in file including sources."
  (nvp-parse:buffer-file nil nil args
    (sh-comp-candidates 'variables file)))

(cl-defmethod nvp-parse-includes ((_mode (eql sh-mode)) &rest args)
  "Sourced files, recursively."
  (nvp-parse:buffer-file nil nil args
    (sh-comp-candidates 'sources file)))

(defun nvp-sh-tidy-buffer ()
  "Cleanup whitespace and align stuff."
  (interactive)
  (unless (or buffer-read-only (null (buffer-file-name)) (not (buffer-modified-p)))
    (delete-trailing-whitespace)
    (ignore-errors (align (point-min) (point-max)))
    (and (buffer-modified-p)
         (save-buffer))))

(provide 'nvp-sh)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-sh.el ends here
