;;; nvp-go.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'go-mode nil t)
(require 'xref)
(nvp:decls :p (go))
(nvp:auto "f" f-base)

(define-advice godef-jump (:after (&rest _args) "pulse")
  (run-hooks 'xref-after-jump-hook))

;;; Newline dwim
(nvp:defmethod nvp-newline-dwim-comment (syntax arg)
  :modes (go-mode go-ts-mode)
  (nvp-newline-dwim--comment syntax arg " * "))

;;; REPL
(with-eval-after-load 'nvp-repl
  (require 'nvp-go-repl))

;;; Yas
(nvp:decl yas-text nvp-yas-split-args)
(defun nvp-go-params (&optional str types join)
  (when-let (str (or str (yas-text)))
    (let ((vals (mapcar
                 (lambda (s) (funcall (if types #'cadr #'car) (split-string s)))
                 (nvp-yas-split-args str))))
      (if join (mapconcat #'identity vals (if (stringp join) join ", "))
        vals))))

(defun nvp-go-fmt-jump ()
  (interactive)
  (compile "go build -v && go test -v && go vet"))

;;; Disassembly
(defun nvp-go-lensm (exe filter)
  (interactive
   (list (read-file-name "Executable: " nil nil nil (f-base (buffer-file-name)))
         (read-string "Filter: " (go--function-name))))
  (unless (executable-find "lensm")
    (user-error "Need to install lensm: nvp build lensm"))
  (start-process-shell-command
   "lensm" nil (format "lensm -watch -filter %s %s" filter exe)))

;;; Tree-sitter
(defvar nvp-go-ts--builtin-functions
  '("append" "cap" "clear" "close" "complex" "copy" "delete" "imag" "len" "make"
    "max" "min" "new" "panic" "print" "println" "real" "recover"))

(defvar nvp-go-ts--builtin-types
  '("any" "bool" "byte" "comparable" "complex128" "complex64" "error" "float32"
    "float64" "int" "int16" "int32" "int64" "int8" "rune" "string" "uint"
    "uint16" "uint32" "uint64" "uint8" "uintptr"))

;; FIXME: remove when ops added
(eval-when-compile (require 'go-ts-mode nil t)) ; `go-ts-mode--operators'

(defvar nvp-go-ts-font-lock-settings
  (when (require 'go-ts-mode nil t)
    (treesit-font-lock-rules
     ;; FIXME: operators not added to `go-ts-mode--font-lock-settings'
     :language 'go
     :feature 'operator
     `([,@go-ts-mode--operators] @font-lock-operator-face)

     :language 'go
     :feature 'builtin
     ;; :override t
     `((call_expression
        function: ((identifier) @font-lock-builtin-face
                   (:match ,(rx-to-string
                             `(seq bos (or ,@nvp-go-ts--builtin-functions) eos))
                           @font-lock-builtin-face)))))))

(with-eval-after-load 'go-ts-mode
  (let* ((features (--map (nth 2 it) nvp-go-ts-font-lock-settings))
         (rules (--filter (not (memq (nth 2 it) features))
                          go-ts-mode--font-lock-settings)))
    (setq go-ts-mode--font-lock-settings
          (append nvp-go-ts-font-lock-settings rules))))

(nvp:run-once go-ts-mode (:after (&rest _))
  (dolist (v '(builtin namespace operator))
    (cl-pushnew v (cadddr treesit-font-lock-feature-list)))
  (treesit-font-lock-recompute-features))

(provide 'nvp-go)
;;; nvp-go.el ends here
