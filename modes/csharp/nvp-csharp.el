;;; nvp-csharp.el --- charp -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (defvar nvp-csc-program))
(require 'nvp)
(nvp:decl nvp-one-shot-keybinding)

(defun nvp-csharp-compile (&optional arg)
  (interactive "P")
  (let* ((args (or (and arg
                        (nvp-one-shot-keybinding
                         "?"
                         'nvp-csharp-compiler-help
                         #'(lambda () (eq major-mode 'minibuffer-inactive-mode)))
                        (read-from-minibuffer "Compiler Arguments(\"?\"): "))
                   " "))
         (compilation-read-command nil))
    (compile (concat nvp-csc-program " " args " " (buffer-file-name)))))

(defun nvp-csharp-compiler-help ()
  "Open compiler options in browser."
  (interactive)
  (browse-url "https://msdn.microsoft.com/en-us/library/6ds95cz0.aspx"))

(provide 'nvp-csharp)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-csharp.el ends here
