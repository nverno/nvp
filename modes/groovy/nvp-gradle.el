;;; nvp-gradle.el --- gradle-mode ext -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls :f (groovy-mode))
(require 'groovy-mode nil t)

(defvar nvp-gradle-imenu-regexp
  (list (list "Tasks" "task[ \t]+\\([a-zA-Z][a-zA-Z0-9_]*\\)" 1)))

;;;###autoload
(define-derived-mode gradle-mode groovy-mode "Gradle"
  "Major mode for editing gradle script."
  (make-local-variable 'groovy-font-lock-keywords)
  (add-to-list
   'groovy-font-lock-keywords
   '("task[ \t]+\\([a-zA-Z][a-zA-Z0-9_]*\\)" 1 font-lock-function-name-face) t)

  (setq imenu-generic-expression nvp-gradle-imenu-regexp))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gradle\\'" . gradle-mode))

(provide 'nvp-gradle)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-gradle.el ends here
