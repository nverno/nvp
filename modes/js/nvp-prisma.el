;;; nvp-prisma.el --- prisma -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:

(defun nvp-prisma-format-buffer ()
  "Run prisma format on current prisma buffer."
  (interactive)
  (let* ((buf (buffer-file-name))
         (pkg (and buf (locate-dominating-file buf "package.json")))
         (prisma (and pkg (expand-file-name "node_modules/.bin/prisma" pkg))))
    (if (and prisma (file-exists-p prisma))
        (shell-command (concat prisma " format --schema=" buf))
      (message "prisma not found"))))

;; (defvar prisma-imenu-generic-expression
;;   '((nil "^\\s-*\\(?:model\\|enum\\)\\s-+\\([[:alnum:]]+\\)\\s-*{" 1)))

(provide 'nvp-prisma)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-prisma.el ends here
