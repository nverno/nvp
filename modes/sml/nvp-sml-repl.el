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
  (let ((buf (save-window-excursion
               (if arg (call-interactively #'sml-run)
                 (funcall #'sml-run
                          sml-program-name sml-default-arg sml-host-name)))))
    ;; For `sml-prog-proc-*' functions
    (setq-local sml-prog-proc-descriptor sml-pp-functions
                sml-prog-proc--buffer buf)
    (get-buffer-process buf)))

(nvp-repl-add '(sml-mode sml-ts-mode)
  :name 'sml
  :modes '(inferior-sml-mode sml-prog-proc-comint-mode sml-prog-proc-mode)
  :init #'nvp-sml-repl-init
  :procname (rx bos "sml" eos)
  :send-string #'sml-prog-proc-send-string
  :send-region #'sml-prog-proc-send-region
  :send-buffer #'sml-prog-proc-send-buffer
  :send-file #'sml-prog-proc-load-file
  :cd-cmd "OS.FileSys.chDir \"%s\";"     ; #'sml-prog-proc-chdir
  :pwd-cmd "OS.FileSys.getDir();"
  :load-cmd "use \"%s\";")

(provide 'nvp-sml-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-sml-repl.el ends here
