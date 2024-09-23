;;; nvp-sml-repl.el --- SML Repl -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-repl)
(nvp:decls :p (sml))

(defun nvp-sml-inf-newline ()
  (interactive)
  (end-of-line)
  (insert ";")
  (comint-send-input))

(defun nvp-sml-repl-init (&optional arg)
  (interactive "P")
  (-> (save-window-excursion
        (if arg (call-interactively #'sml-run)
          (funcall #'sml-run sml-program-name sml-default-arg sml-host-name)))
      (get-buffer-process)))

(nvp-repl-add '(sml-mode sml-ts-mode)
  :name 'sml
  :modes '(inferior-sml-mode sml-prog-proc-comint-mode)
  :init #'nvp-sml-repl-init
  :find-fn (lambda () (ignore-errors (sml-prog-proc-buffer)))
  :send-string #'sml-prog-proc-send-string
  :send-region #'sml-prog-proc-send-region
  :send-buffer #'sml-prog-proc-send-buffer
  :send-file #'sml-prog-proc-load-file
  :cd-cmd "OS.FileSys.chDir \"%s\""     ; #'sml-prog-proc-chdir
  :load-cmd "use \"%s\""
  :pwd-cmd nil)

(provide 'nvp-sml-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-sml-repl.el ends here
