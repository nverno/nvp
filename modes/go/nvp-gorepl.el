;;; nvp-gorepl.el --- Go REPL -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls :p (gorepl))

(with-eval-after-load 'nvp-repl
  (when (fboundp #'gorepl-run-load-current-file)
    (nvp-repl-add '(go-mode go-ts-mode)
      :name 'go
      :modes '(gorepl-mode)
      :bufname gorepl-buffer-name
      ;; :send-input #'gorepl-eval
      :send-buffer #'gorepl-run-load-current-file
      :history-file ".gore_history"
      :help-cmd '(:no-arg ":help" :with-arg ":doc %s")
      :init (lambda (&optional _prefix)
              (save-window-excursion
                (gorepl--run-gore '("-autoimport"))
                (get-buffer-process (current-buffer)))))))

(provide 'nvp-gorepl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-gorepl.el ends here
