;;; nvp-tsx.el ---  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'web-mode nil t)
(require 'nvp-ts)
(nvp:decls :p (web) :f (web-mode))

;; @see https://github.com/syl20bnr/spacemacs/layers/+lang/typescript/packages.el
;;;###autoload
(define-derived-mode typescript-tsx-mode web-mode "Tsx")

(provide 'nvp-tsx)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-tsx.el ends here
