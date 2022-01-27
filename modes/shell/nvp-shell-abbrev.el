;;; nvp-shell-abbrev.el --- shell abbrevs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; TODO: Add option to merge to tables
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'abbrev)
(require 'nvp-shell)
(nvp-req 'nvp-shell 'subrs)
(nvp-decls :f (nvp-abbrev-expand-not-after-punct-p)
           :v (nvp-shell-abbrev-table shells-abbrev-table))

;; read aliases from bash_aliases to alist ((alias . expansion) ... )
(defun nvp-shell-read-aliases (file &optional merge os)
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (let (res sys win)
      (while (not (eobp))
        ;; basic check: assume it is if [[ $OS == ".+" ]]
        ;; only dealing with "Windows_NT", and doesn't
        ;; try to deal with nested ifs
        (if (looking-at
             ;; eval-when-compile
             (nvp:concat "if[^!]*\\(!\\)? *\$OS.*=="
                         "\\s-*[\"']?\\([A-Za-z_0-9]+\\)"))
            (pcase (match-string-no-properties 2)
              (`"Windows_NT"
               (setq sys (if (match-string 1) 'other 'windows)))
              (_
               (setq sys (if (match-string 1) 'windows 'other))))
          (when (search-forward "alias" (line-end-position) t)
            (and (looking-at "[ \t]*\\([^=]+\\)='\\([^']+\\)'")
                 (push (list (match-string-no-properties 1)
                             (match-string-no-properties 2))
                       (if (eq sys 'windows) win res)))))
        ;; reset OS
        (when (looking-at-p "fi")
          (setq sys nil))
        ;; next line
        (forward-line 1))
      (if merge
          ;; use all aliases regardless of system type
          (nconc res win)
        (if os
            (pcase os
              ('windows win)
              (_ res))
          res)))))

;; Make abbrevs from bash_aliases file
;; If MERGE, use all abbrevs regardless of any if [[ $OS == ... ]]
;; If OS == 'windows, use only abbrevs in
;;   if [[ $OS == "Windows_NT" ]] blocks
;; Otherwise, use all others
;;;###autoload
(defun nvp-shell-make-abbrevs (file &optional merge os system)
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
    (nvp-shell-read-aliases file merge os)
    :parents (list shells-abbrev-table prog-mode-abbrev-table)
    :enable-function 'nvp-abbrev-expand-not-after-punct-p
    :regexp nvp-shell-abbrev-re)
  (when system
    (mapatoms (lambda (abbrev)
                (abbrev-put abbrev :system t))
              nvp-shell-abbrev-table))
  ;; Set new abbrev table as local abbrev table
  (setq-local local-abbrev-table nvp-shell-abbrev-table))

;; write shell abbrevs
;; temporarily rebind `abbrev--write' so we can write out
;; :system abbrevs as well
(defun shell-tools-write-abbrevs (file)
  "Write shell abbrevs table to FILE."
  (interactive (list (read-file-name "Write abbrevs to: ")))
  (let ((abbrev-table-name-list '(nvp-shell-abbrev-table)))
    (cl-letf (((symbol-function 'abbrev--write)
               (lambda (sym)
                 (unless (null (symbol-value sym))
                   (insert "    (")
                   (prin1 (symbol-name sym))
                   (insert " ")
                   (prin1 (symbol-value sym))
                   (insert " ")
                   (prin1 (symbol-function sym))
                   (insert " :system t)\n")))))
      (write-abbrev-file file ))))

(provide 'nvp-shell-abbrev)
;;; nvp-shell-abbrev.el ends here
