;;; js-spec-mode.el --- spec files -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp:decls :v (js2-global-externs yas-extra--modes))

(defconst js-spec-global-externs
  '("it" "describe" "expect" "beforeEach" "afterEach" "beforeAll" "afterAll"
    "spyOn"))

;;;###autoload
(define-minor-mode js-spec-mode
  "Minor mode for js specs."
  :lighter " JSpec"
  (if js-spec-mode
      (progn
        (yas-activate-extra-mode 'js-spec-mode)
        (make-local-variable 'js2-global-externs)
        (cl-callf append js2-global-externs js-spec-global-externs))
    (when (boundp 'yas--extra-modes)
      (setq yas--extra-modes (delq 'js-spec-mode yas-extra--modes)))))

(provide 'js-spec-mode)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; js-spec-mode.el ends here
