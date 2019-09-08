;;; awk-repl.el --- awk REPL -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'comint)
(require 'nvp)

(defun awk-repl-start ()
  (with-current-buffer (apply #'make-comint "awk" "awk")))

(define-derived-mode awk-repl-mode comint-mode "Awk"
  "Awk REPL mode.")

(provide 'awk-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; awk-repl.el ends here
