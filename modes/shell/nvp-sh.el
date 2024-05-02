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
(nvp:req 'nvp-shell 'subrs)
(nvp:decls :f (company-shell
               sh-comp-completion-at-point sh-comp-candidates sh-comp--xref-backend))

(nvp:auto "nvp-sh-help" 'nvp-sh-quickhelp-toggle 'nvp-sh-company-show-doc-buffer)

;; for jumping b/w functions -- see `sh-imenu-generic-expression'
(eval-and-compile
  (defconst nvp-sh-function-re
    (nvp:concat
     "\\(?:"
     ;; function FOO()
     "^\\s-*function\\s-+\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*\\(?:()\\)?"
     "\\|"
     ;; FOO()
     "^\\s-*\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*()"
     "\\)")))

;; imenu header comment regexp
(defconst nvp-sh-comment-headers-re '((nil "^###\\s-*\\(.+\\)\\s-*$" 1)))

;; additional imenu regexps
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

;;; Company Quickhelp remappings
(defvar nvp-sh-company-active-map
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km company-active-map)
    ;; (define-key km [remap nvp-company-quickhelp-toggle] #'nvp-sh-quickhelp-toggle)
    (define-key km [remap company-show-doc-buffer] #'nvp-sh-company-show-doc-buffer)
    km))

;; -------------------------------------------------------------------
;;; Navigation
;; commands to enable `beginning-of-defun', `end-of-defun', `narrow-to-defun',
;; etc. to work properly in sh buffers

(defun nvp-sh--beginning-of-defun (&optional arg)
  "Internal implementation for function navigation.
With positive ARG search backwards."
  (when (or (null arg) (= arg 0)) (setq arg 1))
  (let ((search-fn (if (> arg 0) #'re-search-backward #'re-search-forward))
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
      ;; Note: will error on malformed sexps
      (forward-list)
      (point))))

;; -------------------------------------------------------------------
;;; Font-locks

;; Add font-locking & register additions
(nvp:font-lock-add-defaults 'sh-mode
  ;; gaudy array faces
  ("\\${\\([!#?]?[[:alpha:]_][[:alnum:]_]*\\[[@*]\\]\\)}"
   (1 'nvp-special-variable-face prepend))
  ;; redirections
  ("\\([0-9&<>]*[ ]*/dev/null\\)" (1 'nvp-special-type-face prepend))
  ;; doxy params
  ("^# *\\(@[[:alpha:]]+\\)\\s-*\\(\$[[:digit:]]\\)\\(.*\\)$"
   (1 font-lock-doc-face prepend)
   (2 'nvp-special-variable-face prepend)
   (3 font-lock-doc-face prepend))
  ;; quoted vars, special vars, function arguments
  (:quoted ?\" "\\${?\\([[:alpha:]_][[:alnum:]_]*\\|[-#?@!*]\\|[0-9]\\)"
           (1 font-lock-variable-name-face prepend))
  ;; first function in quoted backquote expressions, "`cmd ...`"
  (:quoted ?\" "`\\s-*\\([[:alnum:]_\\-]+\\)" (1 'sh-quoted-exec prepend)))

;; -------------------------------------------------------------------
;;; REPL

;; Drop-in replacement for `sh-shell-process'
(defun nvp-sh-get-process (&optional this-buffer)
  "Return a shell process specific to THIS-BUFFER if non-nil.
Replacement for `sh-shell-process', which see."
  (let ((bufname (and this-buffer
                      (concat "*shell: " (buffer-name (current-buffer)) "*"))))
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

;; overwrite the default function value
(setf (symbol-function 'sh-shell-process) 'nvp-sh-get-process)

;; FIXME: remove, this is covered by generic REPL interface
;; send selected region and step
(defun nvp-sh-send-region (beg end)
  "Send selected region from BEG to END to associated shell process."
  (interactive "r")
  (comint-send-string
   (nvp-sh-get-process) (concat (buffer-substring beg end) "\n"))
  (goto-char end))

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

;; enforce uft-8-unix and align when killing buffer
(defun nvp-sh-tidy-buffer ()
  (interactive)
  (unless (or buffer-read-only (null (buffer-file-name)) (not (buffer-modified-p)))
    (delete-trailing-whitespace)
    (ignore-errors (align (point-min) (point-max)))
    (and (buffer-modified-p)
         (save-buffer))))

(provide 'nvp-sh)
;;; nvp-sh.el ends here
