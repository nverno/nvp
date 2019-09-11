;;; nvp-powershell.el --- powershell utilities  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp-decls)

(defvar nvp-powershell--dir nil)
(defvar nvp-powershell-abbrev-table nil)
(when load-file-name
  (setq nvp-powershell--dir (file-name-directory load-file-name))
  (setq nvp-powershell-abbrev-table
        (expand-file-name "powershell-abbrev-table" nvp-powershell--dir)))

;; ------------------------------------------------------------

;; generate abbrevs from global functions / predefined aliases
(defun nvp-powershell-make-sys-abbrevs (file)
  (interactive
   (list (read-file-name "Write abbrev file: "
                         nvp-powershell--dir "powershell-abbrev-table")))
  (when (file-exists-p file)
    (and (y-or-n-p (format "%s exists, delete? " file))
         (delete-file file)))
  (start-process
   "powershell" "*nvp-powershell*" "powershell" "-f"
   (expand-file-name "script/Get-Abbrevs.ps1" nvp-powershell--dir)
   (expand-file-name "powershell-abbrev-table" nvp-powershell--dir)))

;; FIXME: use Invoke-RequireAdmin.ps1
;; Compile current script with admin priviledge.
(defun nvp-powershell-compile-admin ()
  (interactive)
  (let ((compile-command
         (format "powershell -f %s -asAdmin -params \"-File %s\""
                 (expand-file-name "script/Invoke-RequireAdmin.ps1"
                                   nvp-powershell--dir)
                 buffer-file-name))
        (compilation-read-command))
    (call-interactively 'compile)))

(provide 'nvp-powershell)
;;; nvp-powershell.el ends here
