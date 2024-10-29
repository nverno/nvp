;;; nvp-go.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'xref)
(nvp:decls :p (go))


(with-eval-after-load 'nvp-repl (require 'nvp-go-repl))

(define-advice godef-jump (:after (&rest _args) "pulse")
  (run-hooks 'xref-after-jump-hook))

;;; Dwim
;; Newline
(nvp:defmethod nvp-newline-dwim-comment (syntax arg)
  :modes (go-mode go-ts-mode)
  (nvp-newline-dwim--comment syntax arg " * "))

(cl-defgeneric nvp-go-in-literal-p (&optional _marker _ppss)
  "Return non-nil if in literal."
  (go--in-composite-literal-p))

;; Insert ':' dwim
(defun nvp-go-:-dwim (marker)
  "Insert dwim for \":\" at MARKER in Go."
  (interactive (list (point-marker)))
  (let* ((ppss (syntax-ppss marker))
         (pos (nth 1 ppss)))
    (cond ((or (nth 4 ppss) (nth 3 ppss)) (insert ":"))
          ;; Inside [ .. ] or literal (eg. Foo{ .. })
          ((or (memq (char-after pos) '(?\[))
               (nvp-go-in-literal-p marker ppss))
           (or (eq ?: (char-before))
               (insert ":")))
          (t (delete-horizontal-space)
             (cond ((or (bolp)
                        (eq ?, (char-before)))
                    (indent-according-to-mode)
                    (and (eq ?, (char-before))
                         (insert " "))
                    (yas-expand-snippet
                     (yas-lookup-snippet "assign" 'go-mode)))
                   (t (pcase (char-before)
                        ('nil nil)      ; bob
                        (?: (insert "= "))
                        (?= (delete-char -1)
                            (when (and (eq last-command this-command)
                                       (eq ?: (char-before)))
                              (delete-char -1)
                              (insert "= ")))
                        (_ (insert " := ")))))))))

;;; Yas
(nvp:decl yas-text nvp-yas-split-args)
(defun nvp-go-params (&optional str types join)
  (when-let* ((str (or str (yas-text))))
    (let ((vals (mapcar
                 (lambda (s) (funcall (if types #'cadr #'car) (split-string s)))
                 (nvp-yas-split-args str))))
      (if join (mapconcat #'identity vals (if (stringp join) join ", "))
        vals))))

;;; Disassembly
(defun nvp-go-lensm (exe filter)
  (interactive
   (list (read-file-name
          "Executable: " nil nil nil
          (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
         (read-string "Filter: " (go--function-name))))
  (unless (executable-find "lensm")
    (user-error "Need to install lensm: nvp build lensm"))
  (start-process-shell-command
   "lensm" nil (format "lensm -watch -filter %s %s" filter exe)))

(defun nvp-go-fmt-jump ()
  (interactive)
  (compile "go build -v && go test -v && go vet"))

(provide 'nvp-go)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-go.el ends here
