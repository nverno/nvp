;;; nvp-js.el --- Javascript -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Help refs:
;; - React: https://reactjs.org/docs/{react-component.html}
;; - npm docs <library> name launches site in browser
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'js)
(nvp:req 'nvp-js 'subrs)
(nvp:decls :p (js2 ecma) :f (nvp-js-jsx-hook js2-minor-mode))
           

(nvp:defmethod nvp-newline-dwim-comment (syntax arg)
  ;; Other modes, js-jsx, js2-jsx, rjsx inherit from js/js2
  :modes (js-mode js-ts-mode)           ; js2-mode js3-mode rjsx-mode
  (nvp-newline-dwim--comment syntax arg " * "))

;;; REPLs
(with-eval-after-load 'nvp-repl
  (require 'nvp-nodejs)
  (require 'nvp-skewer))

;;; Toggle b/w JsX <=> JS
;; JsX options (3/7/20): rjsx (better), or js-jsx-mode w/ js2-minor-mode
(defsubst nvp-js--switch-mode (new-mode)
  (kill-all-local-variables)
  (funcall-interactively new-mode))

(defun nvp-js-toggle-jsx (&optional old-mode)
  "Toggle b/w js and jsx modes."
  (interactive)
  (with-demoted-errors "Toggle modes: %S"
    (pcase (or old-mode major-mode)
      ;; => js-jsx-mode w/ js2-minor-mode
      ('rjsx-mode (nvp-js--switch-mode 'js-mode)
                  (nvp-js-jsx-hook))
      ('js2-mode (unless (fboundp 'rjsx-mode)
                   (user-error "rjsx-mode not installed..."))
                 (nvp-js--switch-mode 'rjsx-mode))
      ((or 'js-mode 'js-ts-mode 'js-jsx-mode)
       (unless (fboundp 'rjsx-mode)
         (user-error "rjsx-mode not installed..."))
       (when (bound-and-true-p js2-minor-mode)
         (js2-minor-mode -1))
       (nvp-js--switch-mode 'rjsx-mode))
      (_ (user-error "%S not matched against any JsX modes" major-mode)))))

(defun nvp-js-jsx-file-p ()
  "Enable rsjx mode using `magic-mode-alist'."
  (when buffer-file-name
    (and (member (file-name-extension buffer-file-name) '("js" "jsx"))
         (re-search-forward
          "\\(^\\s-*import React\\|\\( from \\|require(\\)[\"']react\\)"
          magic-mode-regexp-match-limit t)
         (not (nvp:ppss 'soc nil (match-beginning 1))))))

(add-to-list 'magic-mode-alist '(nvp-js-jsx-file-p . rjsx-mode))

;;; Snippets

(defun nvp-js-test-p ()
  "Non-nil when in test directory or buffer."
  (or (string-match-p "\\(?:test\\|spec\\)" (nvp:dfn))
      (string-match-p ".*test\\.js\\'" (nvp:bfn))))

(defun nvp-js-in-template-p (&optional point)
  "Non-nil when in template string `...`."
  (eq ?\` (save-excursion (nvp:ppss 'str nil point))))

;; -------------------------------------------------------------------
;;; Patch `js-ts-mode'

(nvp:decl ecma-ts-merge-rules)
(setq js--treesit-font-lock-settings
      (ecma-ts-merge-rules
       'javascript js--treesit-font-lock-settings 'jsdoc))

(nvp:treesit-add-rules js-ts-mode
  :extra-features '(variable builtin namespace preproc expression))

(provide 'nvp-js)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-js.el ends here
