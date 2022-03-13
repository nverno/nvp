;;; nvp-prisma.el --- prisma -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp:decls :f (projectile-acquire-root add-node-modules-path))

(defun nvp-prisma-format-buffer ()
  (interactive)
  (--when-let (--> (locate-dominating-file (buffer-file-name) "package.json")
                   (and it (expand-file-name "node_modules/.bin/prisma" it)))
    (when (file-exists-p it)
      (shell-command (concat it " format --schema="(buffer-file-name))))))

(defvar prisma-imenu-generic-expression
  '((nil "^\\s-*\\(?:model\\|enum\\)\\s-+\\([[:alnum:]]+\\)\\s-*{" 1)))

(provide 'nvp-prisma)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-prisma.el ends here
