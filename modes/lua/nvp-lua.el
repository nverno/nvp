;;; nvp-lua.el --- lua extensions -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'lua-ts-mode nil t)
(nvp:decls :p (lua) :v (nvp-repl--display-action))

;;; Tree-sitter
(with-eval-after-load 'lua-ts-mode
  (setq lua-ts--font-lock-settings
        (append
         (treesit-font-lock-rules
          :language 'lua
          :feature 'string
          '((string) @font-lock-string-face)
          :language 'lua
          :feature 'escape
          :override t
          '((escape_sequence) @font-lock-escape-face))
         (seq-filter (lambda (e)
                       (not (memq (nth 2 e) '(escape string))))
                     lua-ts--font-lock-settings))))
;;; Repl

(define-advice lua-ts-inferior-lua (:around (orig-fn) "dont-be-dumb")
  (let ((display-buffer-overriding-action nvp-repl--display-action))
    (save-window-excursion
      (funcall orig-fn))))

(with-eval-after-load 'nvp-repl
  (nvp-repl-add '(lua-mode lua-ts-mode)
    :name 'lua
    :send-region #'lua-ts-send-region
    :send-buffer #'lua-ts-send-buffer
    :send-file #'lua-ts-send-file
    :bufname (regexp-quote lua-ts-inferior-buffer)
    :cd-cmd "lfs=require 'lfs'; lfs.chdir(\"%s\")"
    :pwd-cmd "lfs=require 'lfs'; print(lfs.currentdir())"
    :history-file ".lua_history"
    :init (lambda (&optional _prefix)
            (save-window-excursion
              (let ((display-buffer-overriding-action nvp-repl--display-action))
                (funcall-interactively #'lua-ts-inferior-lua))))))

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
