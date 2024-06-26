;;; nvp-bison.el --- bison helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'bison-mode nil t)
;; semantic bunghole
(nvp:decls :v (bison-imenu-regex))

;;;###autoload
(defun nvp-bison-load-imenu ()
  (interactive)
  (setq-local imenu-create-index-function 'imenu-default-create-index-function)
  (setq-local imenu-generic-expression bison-imenu-regex))

;; -------------------------------------------------------------------
;;; parser.output

(defvar bison-output-imenu-regex
  (eval-when-compile 
    (let ((opts (regexp-opt '("Grammar" "Terminal" "Nonterminal" "State "))))
      (append
       `((nil ,(concat "^\\(" opts "\\s-*[0-9]*\\s-*\\(?:conflicts\\)?\\)") 1))))))

;;;###autoload
(define-derived-mode bison-output-mode view-mode "ParserOutput"
  "Major mode for parser.output from bison."
  (setq imenu-create-index-function 'imenu-default-create-index-function)
  (setq imenu-generic-expression bison-output-imenu-regex))

;;;###autoload
(add-to-list 'auto-mode-alist '("parser\\.output" . bison-output-mode))

(provide 'nvp-bison)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-bison.el ends here
