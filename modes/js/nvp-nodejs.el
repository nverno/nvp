;;; nvp-nodejs.el --- REPL -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; (3/17/20) not very useful ATM
;;; XXX:
;; - can't ever load anything into it
;; - doesn't support imports AFAICT
;; - doesn't seem to find libraries by default, probably need some config
;; - no redefinition of symbols -- even when let bound or null...
;; - the process seems to randomly interfere with comint, dumping junk in
;;   the console, fuckin w/ the input
;; - the current design involves a barrage of `comint-output-filter-functions'
;;   which fucks with xterm-color processing, no doubt
;;
;; Would it work to use babel to convert es6 modules to requireJS format
;; recognized by the REPL?
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nodejs-repl)
(require 'nvp-js)
(require 'nvp)
(nvp-decls :v (nvp-trace-group-alist))

;; nodejs-repl doesn't manage comint history files
(define-advice nodejs-repl-quit-or-cancel (:before (&rest _) "write-history")
  (comint-write-input-ring))

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
              nvp-trace-group-alist))

(provide 'nvp-nodejs)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-nodejs.el ends here
