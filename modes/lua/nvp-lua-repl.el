;;; nvp-lua-repl.el --- Lua REPL -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-repl)
(require 'inf-lua nil t)
(nvp:decls :p (inf-lua lua) :f (inf-lua-run))


(defun nvp-lua-repl-init (&optional prefix)
  "Launch lua repl.
With single PREFIX arg setup for debugger:
 - doesnt load init file
 - enables compilation minor mode
With two \\[universal-argument] prompt for lua command."
  (let ((process-environment           ; Linenoise is useless in emacs
         (append (list "DBG_NOREADLINE=1")
                 (copy-sequence process-environment))))
    (funcall #'inf-lua-run (equal '(16) prefix) nil
             (and nvp-repl-load-startup-file
                  (or (null prefix) (>= (prefix-numeric-value prefix) 16))
                  inf-lua-startfile))))

(with-eval-after-load 'inf-lua
  (nvp-repl-add '(lua-mode lua-ts-mode)
    :name 'lua
    :modes '(inf-lua-mode)
    :init #'nvp-lua-repl-init
    :find-fn #'inf-lua-process
    :history-file ".lua_history"
    :cd-cmd "lfs=require 'lfs'; lfs.chdir(\"%s\")"
    :pwd-cmd "lfs=require 'lfs'; print(lfs.currentdir())"
    :help-cmd "_G"                      ; TODO: any useful help?
    :eval-filter (lambda (s) (replace-regexp-in-string inf-lua-prompt-continue "" s))))

(provide 'nvp-lua-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-lua-repl.el ends here
