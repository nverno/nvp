;;; nvp-r-package.el --- package helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro))
(require 'nvp-r)

;; ;;;###autoload
;; (defun nvp-r-package-create (name)
;;   (interactive (list (read-from-minibuffer "Package name: ")))
;;   (nvp-with-script (expand-file-name "script/tools.sh" (nvp-package-root))
;;     `(("create" ,name))))

(provide 'nvp-r-package)
;;; nvp-r-package.el ends here
