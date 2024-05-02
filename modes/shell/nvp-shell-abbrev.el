;;; nvp-shell-abbrev.el --- shell abbrevs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; TODO: Add option to merge to tables
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-shell)                    ; nvp-shell-abbrev-re
(require 'abbrev)
(nvp:decls :f (nvp-abbrev-expand-not-after-punct-p)
           :v (nvp-shell-abbrev-table shells-abbrev-table))
(nvp:auto "nvp-shell-alias" nvp-shell-read-file-aliases)


;; Make abbrevs from bash_aliases file
;; If MERGE, use all abbrevs regardless of any if [[ $OS == ... ]]
;; If OS == 'windows, use only abbrevs in
;;   if [[ $OS == "Windows_NT" ]] blocks
;; Otherwise, use all others
;;;###autoload
(defun nvp-shell-abbrev-from-file (file &optional merge os system)
  "Make abbrevs from alias FILE."
  (interactive
   (let* ((file (read-file-name "Bash aliases: " "~" ".bash_aliases"))
          (merge (y-or-n-p "Merge system specific abbrevs?"))
          (os (and (not merge)
                   (y-or-n-p "Use only windows abbrevs?")
                   'windows))
          (system (y-or-n-p "Create system abbrevs?")))
     (list file merge os system)))
  ;; construct abbrev table
  (define-abbrev-table 'nvp-shell-abbrev-table
    (nvp-shell-read-file-aliases file merge os)
    :parents (list shells-abbrev-table prog-mode-abbrev-table)
    :enable-function 'nvp-abbrev-expand-not-after-punct-p
    :regexp nvp-shell-abbrev-re)
  (when system
    (mapatoms (lambda (abbrev)
                (abbrev-put abbrev :system t))
              nvp-shell-abbrev-table))
  ;; Set new abbrev table as local abbrev table
  (setq-local local-abbrev-table nvp-shell-abbrev-table))

;;;###autoload
(defun nvp-shell-abbrev-write (file)
  "Write shell abbrevs table to FILE."
  (interactive (list (read-file-name "Write abbrevs to: ")))
  (funcall #'nvp-abbrev-write-abbrev-table 'nvp-shell-abbrev-table file))

(provide 'nvp-shell-abbrev)
;;; nvp-shell-abbrev.el ends here
