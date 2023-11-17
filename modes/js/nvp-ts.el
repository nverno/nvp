;;; nvp-ts.el --- typescript -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls :p (typescript))

(eval-and-compile
  (defvar nvp-typescript-modes
    '(typescript-mode typescript-ts-mode typescript-tsx-mode tsx-ts-mode)))

(nvp:defmethod nvp-newline-dwim-comment (syntax arg)
   :modes nvp-typescript-modes
   (nvp-newline-dwim--comment syntax arg " * "))

(with-eval-after-load 'nvp-repl
  (require 'nvp-ts-repl))

(provide 'nvp-ts)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-ts.el ends here
