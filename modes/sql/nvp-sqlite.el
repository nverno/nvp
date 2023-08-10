;;; nvp-sqlite.el --- simple sqli completion -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls :f (sqlite-mode sqlite-mode-list-data))

;;;###autoload
(defun nvp-sqlite-list-data ()
  "Expand/collapse all tables in `sqlite-mode' buffer."
  (interactive nil sqlite-mode)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (eq (get-text-property (point) 'sqlite--type) 'table)
        (call-interactively #'sqlite-mode-list-data))
      (forward-line 1))))

;; -------------------------------------------------------------------
;;; Simple SQLi completion for Sqlite commands

(nvp:define-cache-runonce nvp-sqlite-interactive-commands ()
  "List of sqlite interactive commands."
  (split-string
   (shell-command-to-string
    (concat
     "sqlite3 /dev/null '.help' 2>/dev/null "
     "| awk -F' +' '/^[.][[:alpha:]]+/ {print $1}'"))))

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


(provide 'nvp-sqlite)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-sqlite.el ends here
