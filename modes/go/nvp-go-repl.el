;;; nvp-go-repl.el --- Go REPL -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'gorepl-mode nil t)
(require 'nvp-repl)
(nvp:decls :p (gorepl))

(defun nvp-go-repl-init (&optional _prefix)
  (interactive "P")
  (save-window-excursion
    (gorepl--run-gore '("-autoimport"))
    (get-buffer-process (current-buffer))))

(when (fboundp #'gorepl-run-load-current-file)
  (nvp-repl-add '(go-mode go-ts-mode)
    :name 'go
    :modes '(gorepl-mode)
    :bufname gorepl-buffer-name
    :send-buffer #'gorepl-run-load-current-file
    :history-file ".gore_history"
    :help-cmd '(:no-arg ":help" :with-arg ":doc %s")
    :init #'nvp-go-repl-init))

(provide 'nvp-go-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-go-repl.el ends here
