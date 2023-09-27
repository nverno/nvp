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
(nvp:decls :v (ts-comint-buffer) :f (ts-comint-mode run-ts))

(with-eval-after-load 'nvp-repl
  (nvp-repl-add '(typescript-mode typescript-ts-mode typescript-tsx-mode ts-tsx-mode)
    :modes '(ts-comint-mode)
    :bufname "*Typescript"
    :init (lambda ()
            (run-ts nil 'no-switch)
            (get-buffer-process ts-comint-buffer))))

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
