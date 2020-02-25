;;; nvp-js.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;; - REPLs => nodejs, skewer
;; - newline
;; - xrefs => tern / js2-xref
;; - help  => tern
;; - snippet stuff
;; - http server => httpd
;;
;; TODO:
;; - better xref for node_modules - can tern or js2-xref be taught about global
;;   sources?
;; - get 30s of code js/react functions
;;
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-js-ct "compile/nvp-js-ct"))
(require 'js)
(require 'js2-mode nil t)
(nvp-decls :f (nodejs-repl-switch-to-repl
               nodejs-repl-send-region nodejs-repl-send-last-expression
               skewer-eval-print-last-expression skewer-eval-last-expression
               httpd-start
               js2-display-error-list
               tern-get-docs
               nvp-js2-hook nvp-jsx-hook)
           :v (nodejs-repl-process-name httpd-root httpd-port))

;; when in /* continued comments or doxygen, add comment continuation for
;; newline-dwim
(cl-defmethod nvp-newline-dwim-comment
  (syntax arg &context (major-mode js2-mode js2-jsx-mode js-mode js-jsx-mode
                                   rjsx-mode))
  (nvp-newline-dwim--comment syntax arg " * "))

(cl-defmethod nvp-newline-dwim-comment
  (syntax arg &context (major-mode js-mode))
  (nvp-newline-dwim--comment syntax arg " * "))

;;; http server: httpd

(defun nvp-httpd-here ()
  (interactive)
  (setq httpd-root default-directory)
  (httpd-start))

;;; jsx <=> js
;;; FIXME: now using rsjx mode
;; currently jsx uses js-mode with js2-minor-mode
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
;;; Yas
(defun nvp-js-test-p ()
  (or (string-match-p "\\(?:test\\|spec\\)" (nvp-dfn))
      (string-match-p ".*test\\.js\\'" (nvp-bfn))))

;; -------------------------------------------------------------------
;;; Configuration
;;; TODO: move this to projectile project configuration command
(nvp-defvar nvp-js-test-re (regexp-opt '("jest" "mocha" "jasmine") t))

;; set local values, eg. in .dir-locals.el
(defun nvp-js-local-config (&optional dir)
  (when-let ((default-directory
               (nvp-project-root
                (or dir buffer-file-name default-directory))))
    (nvp-async-shell-command-to-string
     "npm list --depth=0 --only=dev --parseable | awk -F/ '{print $NF}'"
     `(lambda (p res)
        (let ((default-directory ,default-directory))
          (when (zerop (process-exit-status p))
            (unwind-protect
                (with-current-buffer (process-buffer p)
                  (goto-char (point-min))
                  (when (re-search-forward nvp-js-test-re nil t)
                    (let ((test (match-string 1)))
                      (save-window-excursion
                        (add-dir-local-variable nil 'nvp-test-framework test)
                        (save-buffer))
                      (message "Project: %s\nTest framework: %s"
                               (abbreviate-file-name default-directory)
                               test))))
              (kill-buffer (process-buffer p)))))))))

;; -------------------------------------------------------------------
;;; REPLs

;;; Nodejs REPL
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

(defun nvp-js-help-at-point ()
  (interactive)
  (cond
   ((member 'js2-echo-error (get-text-property (point) 'cursor-sensor-functions))
    (js2-display-error-list))
   (t (tern-get-docs))))

(provide 'nvp-js)
;;; nvp-js.el ends here
