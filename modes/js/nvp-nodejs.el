;;; nvp-nodejs.el --- Node REPL -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; FIXME:
;; - doesn't support imports AFAICT => (11/23/20) node doesn't
;; - doesn't seem to find libraries by default, probably need some config
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-repl)
(require 'nodejs-repl nil t)
(nvp:decls :p (nodejs comint))

;; nodejs-repl doesn't manage comint history files
(define-advice nodejs-repl-quit-or-cancel (:before (&rest _) "write-history")
  (comint-write-input-ring))

;;; TODO: dont echo all the .editor stuff in the repl
(defun nvp-nodejs-repl-send-region (process start end)
  "Send the current region from START to END to the `nodejs-repl-process'."
  (interactive "r")
  ;; Enclose the region in .editor ... EOF as this is more robust.
  ;; See: https://github.com/abicky/nodejs-repl.el/issues/17
  (comint-send-string process ".editor\n")
  (comint-send-region process start end)
  (comint-send-string process "\n\x04"))
;; (defun nodejs-repl-send-string-no-output (string &optional ))

(defvar nvp-nodejs--repl-commands
  '("break" "clear" "exit" "help" "save" "load" "editor"))

(when (fboundp 'nodejs-repl--get-or-create-process)
  (nvp-repl-add '(js-mode js-ts-mode js2-mode js2-jsx-mode js-jsx-mode rjsx-mode)
    :name 'nodejs
    :modes '(nodejs-repl-mode)
    :init (lambda (&optional _) (nodejs-repl--get-or-create-process))
    :find-fn (lambda () (get-process nodejs-repl-process-name))
    :history-file ".node_history"
    :send-region #'nodejs-repl-send-region
    :send-buffer #'nodejs-repl-send-buffer
    :send-file #'nodejs-repl-load-file
    :send-sexp #'nodejs-repl-send-last-expression
    :commands nvp-nodejs--repl-commands
    :cmd-prefix ?.
    ;; TODO: reuse but use different command for external config
    ;; :config-cmd #'nvp-typescript-repl-config
    :help-cmd ".help"
    :pwd-cmd "process.cwd()"
    :cd-cmd "process.chdir(\"%s\")"))

(defun nvp-nodejs-region-or-sexp ()
  "Send region or last sexp and step."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'nodejs-repl-send-region)
    (call-interactively 'nodejs-repl-send-last-expression))
  (forward-line))

(provide 'nvp-nodejs)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-nodejs.el ends here
