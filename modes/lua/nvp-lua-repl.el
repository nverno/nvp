;;; nvp-lua-repl.el --- Lua REPL -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-repl)
(require 'lua-ts-mode nil t)
(nvp:decls :p (gorepl lua))

;;; Repl

(define-advice lua-ts-inferior-lua (:around (orig-fn) "dont-be-dumb")
  (let ((display-buffer-overriding-action nvp-repl--display-action))
    (save-window-excursion
      (funcall orig-fn))))

;; Note: `lua-ts-mode' doesn't define a major mode for the REPL
(defvar lua-repl-mode-hook nil)

(nvp-repl-add '(lua-mode lua-ts-mode)
  :name 'lua
  ;; :modes '(lua-repl-mode)
  :send-region #'lua-ts-send-region
  :send-buffer #'lua-ts-send-buffer
  :send-file #'lua-ts-send-file
  :bufname (regexp-quote lua-ts-inferior-buffer)
  :cd-cmd "lfs=require 'lfs'; lfs.chdir(\"%s\")"
  :pwd-cmd "lfs=require 'lfs'; print(lfs.currentdir())"
  :help-cmd "_G"
  :history-file ".lua_history"
  :init (lambda (&optional _prefix)
          (save-window-excursion
            (let* ((display-buffer-overriding-action nvp-repl--display-action)
                   (proc (funcall-interactively #'lua-ts-inferior-lua)))
              (with-current-buffer (process-buffer proc)
                ;; (setq major-mode 'lua-repl-mode)
                (run-hooks 'lua-repl-mode-hook))
              proc))))

(provide 'nvp-lua-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-lua-repl.el ends here
