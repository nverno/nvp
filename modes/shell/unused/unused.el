;;; Shell  -*- no-byte-compile: t; lexical-binding: t -*-
;; -------------------------------------------------------------------
;;; Pcomplete 

(defun pcomplete/shell-mode/git ()
  (pcomplete-here
   '("add" "bisect" "branch" "checkout" "clone" "commit" "diff" "fetch"
     "grep" "init" "log" "merge" "mv" "pull" "push" "rebase" "remote"
     "reset" "rm" "show" "status" "submodule" "tag"))
       
  ;; FIXME: branches? cant use readline on windows, not sure its
  ;; worth fixing
  (pcomplete-here
   (let ((last-cmd (nth (1- pcomplete-last) pcomplete-args)))
     (cond
      ((equal "checkout" last-cmd) " ")
      ;; (my--get-git-branches)
      ((equal "add" last-cmd)
       (pcomplete-entries))
      ((equal "merge" last-cmd) " ")
      ;; (my--get-git-branches t)
      ))))

;; setup clink so it starts whenever cmd.exe runs
(nvp:with-w32
  (defun shell-w32tools-clink-install ()
    (start-process "clink" "*nvp-install*" "cmd.exe"
                   "clink" "autorun" "install")))
