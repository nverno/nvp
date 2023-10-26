;;; nvp-lua.el --- lua extensions -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'lua-ts-mode nil t)
(nvp:decls :p (lua) :v (nvp-repl--display-action))

;;; Repl

(define-advice lua-ts-inferior-lua (:around (orig-fn) "dont-be-dumb")
  (let ((display-buffer-overriding-action nvp-repl--display-action))
    (funcall orig-fn)))

(with-eval-after-load 'nvp-repl
  (nvp-repl-add '(lua-mode lua-ts-mode)
    :send-region #'lua-ts-send-region
    :send-buffer #'lua-ts-send-buffer
    :send-file #'lua-ts-send-file
    :bufname (regexp-quote lua-ts-inferior-buffer)
    :cd-cmd "lfs=require 'lfs'; lfs.chdir(\"%s\")"
    :pwd-cmd "lfs=require 'lfs'; print(lfs.currentdir())"
    ;; :history-file ".lua_history"
    :init (lambda ()
            (save-window-excursion
              (let ((display-buffer-overriding-action nvp-repl--display-action))
                (funcall-interactively #'lua-ts-inferior-lua)
                (nvp-comint-setup-history ".lua_history"))))))

;;; Help

(defun nvp-lua-documentation (thing)
  (interactive (list (read-string "Help: " (lua-funcname-at-point))))
  (let ((url (concat lua-documentation-url "#pdf-" thing)))
    (funcall lua-documentation-function url)))

(provide 'nvp-lua)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-lua.el ends here
