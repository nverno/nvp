;;; nvp-sqlite-completion.el --- simple sqli completion -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp:decls)

(defvar nvp-sqlite--interactive-commands nil)

(defun nvp-sqlite-interactive-commands ()
  (or nvp-sqlite--interactive-commands
      (setq nvp-sqlite--interactive-commands
            (let ((cmds (shell-command-to-string
                         (concat
                          "sqlite3 /dev/null '.help' 2>/dev/null "
                          "| awk -F' +' '/^[.][[:alpha:]]+/ {print $1}'"))))
              (split-string cmds)))))

;;;###autoload
(defun nvp-sqlite-completion-at-point ()
  (let* ((pos (point))
         (beg (condition-case nil
                  (save-excursion
                    (skip-syntax-backward "_w'.")
                    (point))
                (error nil))))
    ;; completion for sqlite shell .<commands>
    (when (and pos beg (eq (char-after beg) ?.))
      (list beg pos
            (completion-table-dynamic
             (lambda (_string) (nvp-sqlite-interactive-commands)))
            :exclusive 'no))))


(provide 'nvp-sqlite-completion)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-sqlite-completion.el ends here
