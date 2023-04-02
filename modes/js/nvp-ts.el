;;; nvp-ts.el --- typescript -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'typescript-mode)
(require 'web-mode)
(nvp:decls)

(cl-defmethod nvp-newline-dwim-comment
  (syntax arg &context (major-mode typescript-mode))
  (nvp-newline-dwim--comment syntax arg " * "))

(cl-defmethod nvp-newline-dwim-comment
  (syntax arg &context (major-mode typescript-tsx-mode))
  (nvp-newline-dwim--comment syntax arg " * "))

;; -------------------------------------------------------------------
;;; REPL
;; > npm i ts-node typescript
;; and this should setup ts-comint to work properly with node module binary
(nvp:decls :v (ts-comint-buffer) :f (ts-comint-mode run-ts add-node-modules-path))

(defun nvp-ts-repl-program ()
  (when (require 'add-node-modules-path nil t)
    ;; npm v >= 9 no longer has 'npm bin' command
    (when (version< (string-trim-right (shell-command-to-string "npm -v")) "9")
      (setq add-node-modules-path-command '("npm bin")))
    (add-node-modules-path))
  (cl-some (lambda (program) (executable-find program)) '("ts-node" "tsun")))

(with-eval-after-load 'nvp-repl
  (nvp-repl-add '(typescript-mode typescript-tsx-mode)
    :modes '(ts-comint-mode)
    :bufname "*Typescript"
    :init (lambda ()
            (run-ts nil 'no-switch)
            (get-buffer-process ts-comint-buffer))))

(defun nvp-ts-get-file-mod (filename)
  (concat "import * as "
          (replace-regexp-in-string
           "^[0-9_]+" ""
           (replace-regexp-in-string "-" "_" (file-name-base filename)))
          " from \""
          (file-name-base filename)
          "\"\n"))

(advice-add 'ts-comint--get-load-file-cmd :override #'nvp-ts-get-file-mod)

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
