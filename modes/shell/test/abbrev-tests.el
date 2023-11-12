;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'nvp-shell)
(require 'nvp-shell-abbrev)

;; (defvar abbrev-tests--dir nil)
;; (when load-file-name
;;   (setq abbrev-tests--dir (file-name-directory load-file-name))
;;   (setq abbrev-tests-aliases (expand-file-name ".bash_aliases"
;;                                                abbrev-tests--dir)))

;; (ert-deftest shell-abbrevs ()
;;   "Should make proper abbrevs."
;;   (let ((abbrevs (shell-abbrevs abbrev-tests-aliases)))
;;     (should (eq abbrevs ))))

(provide 'abbrev-tests)
