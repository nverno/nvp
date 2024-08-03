;;; nvp-sql-repl.el --- Sql repl -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'sql)
(require 'nvp-repl)
(nvp:decls)


(defun nvp-sqli-buffer (&optional _prefix)
  (save-window-excursion
    (sql-show-sqli-buffer)
    (get-buffer-process (current-buffer))))

(defun nvp-sqli-process ()
  (-some->> sql-buffer
    (get-buffer)
    (get-buffer-process)))

(nvp-repl-add '(sql-mode sql-ts-mode sqlite-mode)
  :name 'sql
  :modes '(sql-interactive-mode)
  :init #'nvp-sqli-buffer
  :find-fn #'nvp-sqli-process
  :wait 0.1
  :help-cmd '(:no-arg ".help" :with-arg ".help %s")
  :cd-cmd ".cd \"%s\""
  :pwd-cmd ".shell pwd"
  :config-cmd ".show")


(provide 'nvp-sql-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-sql-repl.el ends here
