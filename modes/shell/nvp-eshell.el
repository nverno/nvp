;;; nvp-eshell.el --- eshell helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; XXX:
;; should be able to use functions/variables from nvp-shell-common now to find
;; active commands/statements at point -- could use that to fix esh-help
;; eldoc function easily.
;;
;;; Code:
(eval-when-compile
  (require 'pcomplete)
  (require 'nvp-macro))
(require 'eshell)
(nvp:decls :f (eshell-send-input eshell-kill-input))
(nvp:auto "pcomplete" pcomplete--here pcomplete-entries)


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

(defun pcomplete/eshell-mode/hg ()
  (pcomplete-here
   '("add" "annotate" "clone" "commit" "diff" "export" "forget" "init"
     "log" "merge" "pull" "push" "remove" "serve" "status" "summary"
     "update")))

;;; Eshell functions
(defun eshell/f (filename &optional dir)
  "Searches in the current directory for files that match the given
 pattern. A simple wrapper around the standard \"find\" function."
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

(defun eshell/ef (filename &optional dir)
  "Searches for the first matching filename and loads it into a file to
 edit."
  (let* ((files (eshell/f filename dir))
         (file (car (split-string files "\n"))))
    (find-file file)))

(defun eshell/x ()
  "Closes the EShell session and gets rid of the EShell window."
  (kill-buffer)
  (delete-window))

(provide 'nvp-eshell)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-eshell.el ends here
