;;; nvp-js.el --- node + webdev support -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Good:
;; ~~~~~
;; - fonts, indentation
;; - autocompletion w/ ternjs
;; - newline DWIM, navigation
;; - JsX support: rjsx is best, then js-js2/js2-minor
;; - generic REPL control
;; - good in project XREF
;; - snippets
;; - flycheck works great, js2-mode is a baws
;;
;; Could be better:
;; ~~~~~~~~~~~~~~~~
;; - tern very good, could be better if:
;;   + jumped to source in node_modules
;;   + used native XREF backend
;;   + parsed the webpack.config.js file without pooping
;;   + returned more + better documentation
;;
;; Pretty shitty:
;; ~~~~~~~~~~~~~~
;; - REPLs => nodejs, skewer, indium (none have worked very well)
;; - Help-at-point => at least jump to help in browser, if not locally available
;; - js2-xref: rarely produces anything very useful -- probably needs config +
;;   it relies on ag/rg, not real parsing
;;
;; Shitty:
;; ~~~~~~
;; - skewer - I've had no luck getting skewer to work with anything more than some
;;   super simple vanilla JS - think I'll just drop it
;; - debugging w/ indium supposed to work with both Chrome and node, but I've
;;   found it to be pretty sporadic, often failing to connect -- my configuration
;;   probably has problems
;;
;; Absent:
;; ~~~~~~~
;; - good documentation - even for builtin JS functions, should at least be able
;;   to easily jump to online documentation
;; - out-of-project XREF, eg. to node_module source code
;; - Any decent project configure/run/test/compile interface
;; - The default npm config with projectile is bare
;; - npm interface - probably not worth the effort, since the shell works just
;;   fine
;;
;; Priorities:
;; ~~~~~~~~~~~
;; 1. Package.json parser:
;;   - get list script targets for projectile-run/test/compile commands
;;   - Create basic projectile npm/react project runners
;;   - Worry about config later
;; 2. Documentation:
;;   - Need help-at-point for both JS functions and React functions
;;   - Opening a browser page would be OK, not ideal
;;   - JS/React/Node .info files available?
;; 3. Teach tern to read webpack.config, in order of goodness:
;;   - wrap server in correct process-environment
;;   - use a tern-shim to set env before starting
;;   - source NODE_ENV=development on login -- not sure it would work
;; 4. Get a decent REPL working (nodejs):
;;   - Need to be able to reevaluate consts -- indium was able to solve that
;;     problem -- look there
;;   - process-filter bug
;;   - random input from REPL breaks user input
;;   - fix blocking, eg. accept-process-output loop or something
;;   - Debugger is less important since devtools are really good
;; 5. Add local binary paths when in project:
;;   - could use .dir-locals.el
;;   - could try direnv -- not sure how it works w/ emacs
;;   - custom configuration, ala conda-env, based on current project
;; 6. Fix JS <=> JsX to favor rjsx
;; 7. Persist project configuartions across sessions:
;;   - .dir-locals.el is only obvious one, but kinda sucks
;;   - Might have to manage another cache, linked to projectiles
;; 8. Add react 31s snippets?  Problem is they may contain CSS as well
;; 9. Move unused stuff:
;;   - skewer I think
;;   - js2-refactor?
;;   - httpd => could all just move over to web, node/python servers are probably
;;     better
;; 10. Possible to stop js2-mode from flychecking shebang line?
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'js)
(require 'js2-mode nil t)
(nvp-req 'nvp-js 'subrs)
(nvp-decls :f (nodejs-repl-switch-to-repl
               nodejs-repl-send-region nodejs-repl-send-last-expression
               skewer-eval-print-last-expression skewer-eval-last-expression
               httpd-start
               js2-display-error-list
               tern-get-docs
               nvp-js2-hook nvp-jsx-hook)
           :v (nodejs-repl-process-name httpd-root httpd-port))

;; when in /* continued comments or doxygen, add comment continuation for
;; newline-dwim -- other modes, js-jsx, js2-jsx, rjsx inherit from js/js2
(cl-defmethod nvp-newline-dwim-comment
  (syntax arg &context (major-mode js-mode js2-mode js3-mode))
  (nvp-newline-dwim--comment syntax arg " * "))

;;; Toggle b/w JsX <=> JS
;; JsX options (3/7/20): rjsx (better), or js-jsx-mode w/ js2-minor-mode
(defun nvp-js-toggle-jsx ()
  "Toggle b/w js and jsx modes."
  (interactive)
  (if (equal mode-name "jsx")
      (progn
        (js2-minor-mode -1)
        (nvp-js2-hook))
    (js2-mode-exit)
    (nvp-jsx-hook)))

;; -------------------------------------------------------------------
;;; Nodejs REPL: using as default in all js-derived modes

(with-eval-after-load 'nvp-repl
  (nvp-repl-add '(js2-mode js2-jsx-mode js-mode js-jsx-mode)
    :modes '(nodejs-repl-mode)
    :procname (bound-and-true-p nodejs-repl-process-name)
    :init (lambda ()
            (save-window-excursion
              (ignore-errors
                (call-interactively #'nodejs-repl-switch-to-repl))))))

(defun nvp-js-nodejs-region-or-sexp ()
  (interactive)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'nodejs-repl-send-region)
      (call-interactively 'nodejs-repl-send-last-expression))
  (forward-line))

;; nodejs-repl doesn't manage comint history files
(define-advice nodejs-repl-quit-or-cancel (:before (&rest _) "write-history")
  (comint-write-input-ring))

;; -------------------------------------------------------------------
;;; Yas

(defun nvp-js-test-p ()
  (or (string-match-p "\\(?:test\\|spec\\)" (nvp-dfn))
      (string-match-p ".*test\\.js\\'" (nvp-bfn))))

;; -------------------------------------------------------------------
;;; other REPLs

;;; Skewer

(defun nvp-skewer-eval-last-expression (&optional print)
  (interactive "P")
  (call-interactively 
   (if print #'skewer-eval-print-last-expression
     #'skewer-eval-last-expression)))

;; -------------------------------------------------------------------
;;; Font locking

;; shebang in node scripts not recognized by js2
(defun nvp-js2-font-lock-additions ()
  (font-lock-add-keywords
   nil
   '(("\\`\\(#!\\s-*.*/[^ \t\n]+\\)\\s-*\\([^ \t\n]+\\)\\s-*$"
      (1 font-lock-comment-face t)
      (2 'nvp-italic-type-face t))))
  (font-lock-flush)
  (font-lock-ensure))

;;; XXX: gets overwritten by stuff js2 does -- not sure why
(defalias 'nvp-js2-syntax-propertize-shebang
  (syntax-propertize-rules
   ("\\`\\(#\\)!.*/[^ \t\n]+" (1 "!"))))

;; -------------------------------------------------------------------
;;; Help
;;
;; - React: https://reactjs.org/docs/{react-component.html}

(defun nvp-js-help-at-point ()
  (interactive)
  (cond
   ((member 'js2-echo-error (get-text-property (point) 'cursor-sensor-functions))
    (js2-display-error-list))
   (t (tern-get-docs))))

;;; http server: httpd

(defun nvp-httpd-here ()
  (interactive)
  (setq httpd-root default-directory)
  (httpd-start))

(provide 'nvp-js)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-js.el ends here
