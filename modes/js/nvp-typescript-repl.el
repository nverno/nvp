;;; nvp-typescript-repl.el --- Typescript REPL -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; > npm i ts-node typescript
;; and this should setup ts-comint to work properly with node module binary
;;; Code:

(eval-when-compile (require 'nvp-macro))
(require 'nvp-repl)
(nvp:decls
 :p (ts add-node) :v (ts-repl-buffer nvp-typescript-modes) :f (ts-repl-run))


(defvar nvp-typescript--repl-commands
  '("break" "clear" "exit" "help" "save" "load" "editor" "type"))

(when (fboundp 'ts-repl-run)
  (require 'ts-repl)
  (nvp-repl-add nvp-typescript-modes
    :name 'typescript
    :modes '(ts-repl-mode)
    :init #'ts-repl-run
    :find-fn #'ts-repl-process
    :send-region #'ts-repl-send-region
    :eval-string #'ts-repl-eval-string
    :commands nvp-typescript--repl-commands
    :cmd-prefix ?.
    :help-cmd '(:no-arg ".help" :with-arg ".type %s")
    :config-cmd #'nvp-typescript-repl-config
    :pwd-cmd "process.cwd()"
    :cd-cmd "process.chdir(\"%s\")"
    :cmd-handlers '(("?" . ".type %s"))
    :history-file ".ts_history"))

(defun nvp-typescript-repl-config (&optional external &rest _)
  (if (and (null external)
           (eq major-mode 'ts-repl-mode))
      (nvp-repl-send-string
       (concat "console.table(util.inspect.styles);"
               "console.table(util.inspect.defaultOptions)"))
    (shell-command "npx ts-node --showConfig")))

(declare-function browse-url-chrome "browse-url")

(defun nvp-typescript-repl-inspect ()
  "Attach chrome debugger to repl."
  (interactive)
  (let ((proc (ts-repl-process))
        (cmd "inspector.url() || inspector.open()\n")
        (browse-url-browser-function #'browse-url-chrome))
    (and proc (comint-send-string proc cmd))
    (message "goto chrome://inspect")
    ;; XXX(10/28/24): doesn't go there..
    (browse-url "chrome://inspect")))

(provide 'nvp-typescript-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-typescript-repl.el ends here
