;;; nvp-ts.el --- typescript -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'typescript-mode nil t)
(require 'web-mode nil t)
(nvp:decls :p (typescript web) :f (web-mode))

(nvp:defmethod nvp-newline-dwim-comment (syntax arg)
  :modes (typescript-mode typescript-ts-mode typescript-tsx-mode ts-tsx-mode)
  (nvp-newline-dwim--comment syntax arg " * "))

;; -------------------------------------------------------------------
;;; REPL
;; > npm i ts-node typescript
;; and this should setup ts-comint to work properly with node module binary

(require 'ts-comint nil t)
(nvp:decls :p (ts add-node) :f (run-ts))

(with-eval-after-load 'nvp-repl
  (nvp-repl-add '(typescript-mode typescript-ts-mode typescript-tsx-mode ts-tsx-mode)
    :name 'typescript
    :modes '(ts-comint-mode)
    :bufname "*Typescript"
    :send-string #'ts-send-string
    :send-region #'ts-send-region
    :send-sexp #'ts-send-last-sexp
    :send-buffer #'ts-send-buffer
    :send-file #'ts-load-file
    :history-file ".ts_history"
    :init (lambda (&optional _prefix)
            (run-ts nil 'no-switch)
            (get-buffer-process ts-comint-buffer))))

;; Note: install `npm i ts-node typescript' in project and this
;; should find the local REPL binary
;;;###autoload
(defun nvp-ts-repl-setup (&optional force)
  (unless (or (null force)
              (and ts-comint-program-command
                   (executable-find ts-comint-program-command)))
    (setq ts-comint-program-command (nvp-ts-repl-program))))

(defun nvp-ts-repl-program ()
  (when (require 'add-node-modules-path nil t)
    ;; npm v >= 9 no longer has 'npm bin' command
    (when (version< (string-trim-right (shell-command-to-string "npm -v")) "9")
      (nvp:setq add-node-modules-path-command '("npm bin")))
    (add-node-modules-path))
  (cl-some (lambda (program) (executable-find program)) '("ts-node" "tsun")))

(defun nvp-ts-repl-get-file-mod (filename)
  (concat "import * as "
          (replace-regexp-in-string
           "^[0-9_]+" ""
           (replace-regexp-in-string "-" "_" (file-name-base filename)))
          " from \""
          (file-name-base filename)
          "\"\n"))

(advice-add 'ts-comint--get-load-file-cmd :override #'nvp-ts-repl-get-file-mod)

;; -------------------------------------------------------------------
;;; Tsx
;; @see https://github.com/syl20bnr/spacemacs/blob/develop/layers/%2Blang/typescript/packages.el

;;;###autoload
(define-derived-mode typescript-tsx-mode web-mode "Tsx")

(provide 'nvp-ts)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-ts.el ends here
