;;; nvp-vc.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; ghub?
;; https://github.com/jwiegley/dot-emacs/blob/0e07f471036d6f3ec4f3cbd38fe3277be072747b/init.el#L180
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'magit nil t)
(nvp-decls :f (git-messenger:popup-message))

;; -------------------------------------------------------------------
;;; Magit

;; kill all the open magit buffers for the current repo
(defun nvp-magit-kill-buffers ()
  (interactive)
  (mapc #'kill-buffer (magit-mode-get-buffers)))

;; -------------------------------------------------------------------
;;; Git Messenger

(define-advice git-messenger:popup-message (:around (fn &rest _) "no-error")
  (with-demoted-errors "GitMsg (not a repo?): %S"
    (let (debug-on-error)               ; no thanks
      (funcall fn))))

;; -------------------------------------------------------------------
;;; Random Git

;; checkout part of a repo
;;;###autoload
(defun nvp-vc-git-sparse-clone (repo subdir local-dir)
  "Clone SUBDIR from REPO to LOCAL-DIR."
  (interactive
   (let* ((repo (read-string "Repository URI: "))
          (subdir (read-string "Subdirectory to clone: "))
          (ddir (car (last (split-string repo "/"))))
          (local-dir (read-directory-name "Clone into: "
                                          (or (getenv "DEVEL")
                                              default-directory)
                                          ddir nil ddir)))
     (list repo subdir local-dir)))
  (unless (file-exists-p local-dir)
    (mkdir local-dir))
  (let ((default-directory local-dir))
    (call-process-shell-command
     (format "git init && git remote add -f origin %s && \
git config core.sparseCheckout true" repo) nil nil nil)
    (with-current-buffer (find-file-noselect ".git/info/sparse-checkout")
      (goto-char (point-max))
      (insert subdir)
      (pop-to-buffer-same-window (current-buffer))
      (add-hook 'after-save-hook
                #'(lambda ()
                    (let ((default-directory `,local-dir))
                      (start-process "git" nil "git" "pull" "origin" "master")))
                nil 'local))))

(provide 'nvp-vc)
;;; nvp-vc.el ends here
