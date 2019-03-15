;;; shell-w32-env.el --- make shell work a little better on windows -*- lexical-binding: t; -*-

;; Author: noah peart

;; This is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary

;; Fix shell completion at point for windows env. variables:
;;  + case-insensitive
;;  + preceded by '%' instead of '$'
;;  + add annotation for company-capf to show actually values
;; of env. variables in minibuffer and completion menu.

;; Usage:
;;
;; In shell-mode hook, if using cmd.exe of cmdproxy.exe
;;
;;    (setq shell-dynamic-complete-functions
;;      '(comint-c-a-p-replace-by-expanded-history
;;        shell-w32-environment-variable-completion
;;        shell-c-a-p-replace-by-expanded-directory
;;        pcomplete-completions-at-point
;;        shell-filename-completion
;;        comint-filename-completion))
;;
;;    ;; and create hook, here using company-shell backend as well
;;
;;    (defun my-shell-hook ()
;;      (setq-local company-backends
;;        '((company-capf company-shell)))
;;      (shell-completion-vars))
;;
;;    (add-hook 'shell-mode-hook #'my-shell-hook)

;;; Code:
(require 'shell)

(defgroup shell-w32 nil
  "Modify shell shell completion-at-point for windows cmd.exe shell."
  :prefix "shell-w32-"
  :group 'shell)

;; (defcustom shell-w32-expand-variables)

;;@@FIXME: if `completion-ignore-case' is non-nil this shouldn't be
;; necessary? but can't figure out how to let-bind it properly
(defun shell-w32--ignore-case (lst)
  "Completion candidates for upper and lower case."
  (append (mapcar #'downcase lst) lst))

(defvar shell-w32-environment-variables nil
  "List of process environment variables.")

(defun shell-w32-match-partial-variable ()
  "Return the shell variable at point, or nil if none is found.
Include a prefix of '%` instead of `$'."
  (save-excursion
    (if (re-search-backward "[^A-Za-z0-9_{(]" nil 'move)
        (or (looking-at "%") (forward-char 1)))
    (if (or (eolp) (looking-at "[^A-Za-z0-9_{(%]"))
        nil
      (looking-at "%?[{(]?[A-Za-z0-9_]*[})]?")
      (buffer-substring (match-beginning 0) (match-end 0)))))

(defun shell-w32-dynamic-environment-variable-completion ()
  "Complete environment variables from `process-environment' at point, 
ignoring case. Completes if after a variable, ie. starts with a \"%\".

The same as `shell-dynamic-complete-environment-variable', except
allowing for the different prefix and case insensitive. 
It also stores the environment variables after the first completion, so
its not really dynamic, which would be annoying if new variables are being 
created."
  (interactive)
  (let ((data (shell-w32-environment-variable-completion)))
    (if data
        (prog2 (unless (window-minibuffer-p)
                 (message "Completing variable name..."))
            (apply #'completion-in-region data)))))

;;;###autoload
(defun shell-w32-environment-variable-completion ()
  "Completion data for  environment variables from `process-environment'"
  (let* ((var (shell-w32-match-partial-variable))
         (end (match-end 0))
         (completion-ignore-case t))
    (when (and (not (zerop (length var))) (eq (aref var 0) ?%)
               ;; don't expand again, it's annoying
               (not (save-excursion
                      (goto-char (match-beginning 0))
                      (unless (bolp) (forward-char -1)
                              (looking-at-p "[[:alnum:]_\\.]")))))
      (let ((start
             (save-excursion
               (goto-char (match-beginning 0))
               (looking-at "%")
               (match-end 0)))
            (variables
             (or shell-w32-environment-variables
                 (progn
                   (setq shell-w32-environment-variables
                         (shell-w32--ignore-case
                          (mapcar (lambda (x)
                                    (upcase
                                     (substring x 0 (string-match "=" x))))
                                  process-environment)))
                   shell-w32-environment-variables))))
        (list start end variables
              :annotation-function (lambda (s) (getenv s))
              :company-docsig (lambda (s) (getenv s))
              :exit-function
              (lambda (s finished)
                (when (memq finished '(sole finished))
                  (insert "%")
                  (if (file-directory-p
                       (comint-directory (getenv s)))
                      (insert "/")))))))))

;;@@FIXME: should be able to do "cd ~" and have it work properly
(defun shell-w32-input-filter (str)
  "Convert \"~\" to expanded %HOME% so cmd.exe gets it right."
  (when (string-match-p "~" str)
    (replace-regexp-in-string "~" (expand-file-name (getenv "HOME")) str))
  str)

;; (defun pcomplete/shell-mode/cd ()
;;   (pcomplete-here
;;    (let ((last-cmd (nth pcomplete-last pcomplete-args)))
;;      (cond
;;       ((equal "~" last-cmd)
;;        (delete-char -1)
;;        (insert (getenv "HOME")))
;;       (t nil)))))

(provide 'shell-w32-env)

;;; shell-w32-env.el ends here
