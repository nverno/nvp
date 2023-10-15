;;; nvp-lua.el --- lua extensions -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls)
(require 'lua-mode)

(with-eval-after-load 'nvp-repl
  (nvp-repl-add '(lua-mode lua-ts-mode)
    :send-string #'lua-send-string
    :send-defun (lambda () (call-interactively #'lua-send-defun))
    :send-line #'lua-send-current-line
    :send-region #'lua-send-region
    :send-buffer #'lua-send-buffer
    :find-fn #'lua-get-create-process
    :init #'lua-get-create-process))

(provide 'nvp-lua)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-lua.el ends here