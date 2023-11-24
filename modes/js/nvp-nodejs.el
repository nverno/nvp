;;; nvp-nodejs.el --- Node REPL -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; FIXME:
;; - doesn't support imports AFAICT => (11/23/20) node doesn't
;; - doesn't seem to find libraries by default, probably need some config
;; - no redefinition of symbols -- even when let bound or null...
;; Would it work to use babel to convert es6 modules to requireJS format
;; recognized by the REPL?
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-repl)
(nvp:decls :p (nodejs) :v (nvp-trace-group-alist nvp-js-modes))

;; nodejs-repl doesn't manage comint history files
(define-advice nodejs-repl-quit-or-cancel (:before (&rest _) "write-history")
  (comint-write-input-ring))

(when (fboundp #'nodejs-repl-switch-to-repl)
  (nvp-repl-add nvp-js-modes
    :name 'nodejs
    :modes '(nodejs-repl-mode)
    :procname (bound-and-true-p nodejs-repl-process-name)
    :send-region #'nodejs-repl-send-region
    :send-buffer #'nodejs-repl-send-buffer
    :send-file #'nodejs-repl-load-file
    :send-sexp #'nodejs-repl-send-last-expression
    :history-file ".node_history"
    :help-cmd ".help"
    :init (lambda (&optional _prefix)
            (save-window-excursion
              (ignore-errors
                (call-interactively #'nodejs-repl-switch-to-repl))))))

(defun nvp-nodejs-region-or-sexp ()
  "Send region or last sexp and step."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'nodejs-repl-send-region)
    (call-interactively 'nodejs-repl-send-last-expression))
  (forward-line))

;; -------------------------------------------------------------------
;;; Find problem with REPL

;; debugging
(with-eval-after-load 'nvp-trace
  (cl-pushnew '(nodejs                  ; all `comint-output-filter-functions'
                nodejs-repl--clear-cache
                nodejs-repl--filter-escape-sequnces
                nodejs-repl--remove-duplicated-prompt
                nodejs-repl--delete-prompt)
              nvp-trace-group-alist
              :test #'member))

(provide 'nvp-nodejs)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-nodejs.el ends here
