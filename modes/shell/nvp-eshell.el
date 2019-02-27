;;; nvp-eshell.el --- eshell helpers -*- lexical-binding: t; -*-
;; Last modified: <2019-01-24 17:53:03>
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'pcomplete))
(require 'eshell)
(require 'nvp-shell)
(declare-function eshell-kill-input "esh-mode")
(declare-function eshell-send-input "esh-mode")

(autoload 'pcomplete--here "pcomplete")
(autoload 'pcomplete-entries "pcomplete")

;;;###autoload
(defun nvp-eshell-this-dir ()
  "Open or move eshell to `default-directory'."
  (interactive)
  (unless (get-buffer eshell-buffer-name)
    (eshell))
  (let ((dir default-directory))
    (switch-to-buffer eshell-buffer-name)
    (goto-char (point-max))
    (eshell-kill-input)
    (insert (format "cd %s" dir)))
  (eshell-send-input)
  (goto-char (point-max)))

;;; Completion

;; FIXME:
;; (defun pcomplete/eshell-mode/gc ()
;;   (pcomplete-here (my--get-git-branches)))

(defun pcomplete/eshell-mode/git ()
  (pcomplete-here
   '("add" "bisect" "branch" "checkout" "clone" "commit"
     "diff" "fetch" "grep" "init" "log" "merge" "mv" "pull"
     "push" "rebase" "remote" "reset" "rm" "show" "status" "tag"))
       
  (pcomplete-here
   (let ((last-cmd (nth (1- pcomplete-last) pcomplete-args)))
     (cond
      ((equal "checkout" last-cmd) " ")
      ;; (esh-get-git-branches)
      ((equal "add" last-cmd)
       (pcomplete-entries))
      ((equal "merge" last-cmd) " ")
      ;; (esh-get-git-branches t)
      ))))

;; hg config

(defun pcomplete/eshell-mode/hg ()
  (pcomplete-here
   '("add" "annotate" "clone" "commit" "diff" "export" "forget" "init"
     "log" "merge" "pull" "push" "remove" "serve" "status" "summary"
     "update")))

;;; eshell functions

;; Searches in the current directory for files that match the
;; given pattern. A simple wrapper around the standard 'find'
;; function.
(defun eshell/f (filename &optional dir)
  (let ((cmd (concat
              "find " (or dir ".")
              "      -not -path '*/.git*'"
              " -and -not -path '*node_modules*'"
              " -and -not -path '*classes*'"
              " -and "
              " -type f -and "
              "-iname '" filename "'")))
    (message cmd)
    (shell-command-to-string cmd)))

;; Searches for the first matching filename and loads it into a
;; file to edit.
(defun eshell/ef (filename &optional dir)
  (let* ((files (eshell/f filename dir))
         (file (car (split-string files "\n"))))
    (find-file file)))

;; Closes the EShell session and gets rid of the EShell window.
(defun eshell/x ()
  (kill-buffer)
  (delete-window))

(provide 'nvp-eshell)
;;; nvp-eshell.el ends here
