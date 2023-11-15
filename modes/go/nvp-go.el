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
  (require 'nvp-gorepl))

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

(provide 'nvp-go)
;;; nvp-go.el ends here
