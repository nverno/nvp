;;; nvp-js.el --- node + webdev support -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Help refs:
;; - React: https://reactjs.org/docs/{react-component.html}
;; - npm docs <library> name launches site in browser
;;
;; TODO:
;; - REPLs => nodejs, skewer, indium
;;   - [✓] process-filter bug -- xterm-color related probably
;;   - [✓] random input from REPL breaks user input
;;   - [✓] fix blocking, eg. accept-process-output loop or something
;;   - Need to be able to reevaluate consts (indium solved this)
;;   - [ ] debugging w/ indium supposed to work with both Chrome and node
;;     Fix configuration to solve failing connections
;; - help-at-point for React
;; - The default npm config with projectile is bare
;; - Package.json parser:
;;   - get list script targets for projectile-run/test/compile commands
;;   - Create basic projectile npm/react project runners
;; - Add react 31s snippets?  Problem is they may contain CSS as well
;;; Code:

(eval-when-compile (require 'nvp-macro))
(require 'js)
(require 'js2-mode nil t)
(nvp:req 'nvp-js 'subrs)
(nvp:decls :p (js2 tern httpd yas)
           :f (nvp-js-jsx-hook nvp-js2-hook nvp-jsx-hook nvp-rjsx-hook))

(defvar nvp-js-modes
  '(js-mode js-ts-mode js2-mode js2-jsx-mode js-jsx-mode rjsx-mode))

;; when in /* continued comments or doxygen, add comment continuation for
;; newline-dwim -- other modes, js-jsx, js2-jsx, rjsx inherit from js/js2
(nvp:defmethod nvp-newline-dwim-comment (syntax arg)
  :modes (js-mode js-ts-mode)           ;js2-mode js3-mode rjsx-mode
  (nvp-newline-dwim--comment syntax arg " * "))

;;; REPLs
(with-eval-after-load 'nvp-repl
  ;; FIXME: indium
  ;; (require 'nvp-indium)
  (require 'nvp-nodejs)
  (require 'nvp-skewer))

;;; Httpd server
(defun nvp-httpd-here ()
  (interactive)
  (setq httpd-root default-directory)
  (httpd-start))

;;; Toggle b/w JsX <=> JS
;; JsX options (3/7/20): rjsx (better), or js-jsx-mode w/ js2-minor-mode
(eval-when-compile
  (defsubst nvp:js-switch-mode (new-mode)
    (kill-all-local-variables)
    (funcall-interactively new-mode)))

(defun nvp-js-toggle-jsx (&optional old-mode)
  "Toggle b/w js and jsx modes."
  (interactive)
  (with-demoted-errors "Toggle modes: %S"
    (pcase (or old-mode major-mode)
      ('rjsx-mode                      ; => js-jsx-mode w/ js2-minor-mode
       (nvp:js-switch-mode 'js-mode)
       (nvp-js-jsx-hook))
      ('js2-mode
       (unless (fboundp 'rjsx-mode) (user-error "rjsx-mode not installed..."))
       (nvp:js-switch-mode 'rjsx-mode))
      ((or 'js-mode 'js-ts-mode 'js-jsx-mode)
       (unless (fboundp 'rjsx-mode) (user-error "rjsx-mode not installed..."))
       (when (bound-and-true-p js2-minor-mode)
         (js2-minor-mode -1))
       (nvp:js-switch-mode 'rjsx-mode))
      (_ (user-error "%S not matched against any JsX modes" major-mode)))))

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

;;; Snippets

(defun nvp-js-test-p ()
  (or (string-match-p "\\(?:test\\|spec\\)" (nvp:dfn))
      (string-match-p ".*test\\.js\\'" (nvp:bfn))))

;; in template string `...`
(defun nvp-js-in-template-p () (eq ?\` (nvp:ppss 'str)))

;;; Setup
;; from spacemacs
(defun nvp-js-jsx-file-p ()
  "Enable rsjx mode using `magic-mode-alist'."
  (when buffer-file-name
    (and (member (file-name-extension buffer-file-name) '("js" "jsx"))
         (re-search-forward
          "\\(^\\s-*import React\\|\\( from \\|require(\\)[\"']react\\)"
          magic-mode-regexp-match-limit t)
         (not (nvp:ppss 'soc nil (match-beginning 1))))))

(add-to-list 'magic-mode-alist '(nvp-js-jsx-file-p . rjsx-mode))

(provide 'nvp-js)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-js.el ends here
