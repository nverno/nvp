;;; nvp-go-repl.el --- Go REPL -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'gorepl-mode nil t)
(require 'nvp-repl)
(nvp:decls :p (gorepl))

(when (featurep 'gorepl-mode)
  (nvp-repl-add '(go-mode go-ts-mode)
    :name 'go
    :modes '(gorepl-mode)
    :init #'gorepl-run
    :find-fn #'gorepl-buffer
    :procname gorepl-buffer-name
    :send-file #'gorepl-load-file
    :history-file ".gore_history"
    :help-cmd '(:no-arg ":help" :with-arg ":doc %s")
    :cmd-handlers '(("?" . ":doc %s"))))

(provide 'nvp-go-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-go-repl.el ends here
