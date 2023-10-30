;;; nvp-go.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; TODO:
;; - delve debugger setup
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'go-mode nil t)
(require 'xref)
(nvp:decls :p (go gorepl))

(define-advice godef-jump (:after (&rest _args) "pulse")
  (run-hooks 'xref-after-jump-hook))

;;; Newline dwim
(nvp:defmethod nvp-newline-dwim-comment (syntax arg)
  :modes (go-mode go-ts-mode)
  (nvp-newline-dwim--comment syntax arg " * "))

;;; REPL

(with-eval-after-load 'nvp-repl
  (with-eval-after-load 'gorepl-mode
    (nvp-repl-add '(go-mode go-ts-mode)
      :name 'go
      :modes '(gorepl-mode)
      :bufname gorepl-buffer-name
      ;; :send-input #'gorepl-eval
      :send-buffer #'gorepl-run-load-current-file
      :history-file ".gore_history"
      :help-cmd (lambda (&optional thing)
                  (nvp-repl-send-string (if thing (concat ":doc " thing) ":help")))
      :init (lambda ()
              (save-window-excursion
                (gorepl--run-gore '("-autoimport"))
                (get-buffer-process (current-buffer)))))))

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

(provide 'nvp-go)
;;; nvp-go.el ends here
