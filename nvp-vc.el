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
(nvp:decls :f (git-messenger:popup-message))

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
;;; Config

;; caches aliases in tabulated list format after first call
(nvp:define-cache nvp-git-aliases ()
  (cl-flet
      ((parse-aliases
        ()
        (let (res)
          (goto-char (point-min))
          (while (not (eobp))
            (and (looking-at "\\([[:alnum:]-_]+\\)\\(.*\\)")
                 (push (list (match-string 1)
                             `[,(match-string 1) ,(match-string 2)])
                       res))
            (forward-line 1))
          (kill-buffer (current-buffer))
          res)))
    (nvp:with-process "git"
      ;; git alias => command in my gitconfig to list them all
      :proc-args ("alias | sed 's|^\\\([^=]*\\\)=\\\(.*\\\)$|\\1\\2|g'") 
      :shell t
      :sync t
      :on-success #'parse-aliases)))

;;;###autoload
(defun nvp-git-list-aliases ()
  "List git aliases in tabulated list format."
  (interactive)
  (nvp:with-tabulated-list
    :name "git-aliases"
    :format [("Alias" 18 t) ("Expansion" 60 t)]
    :entries (nvp-git-aliases)
    :action (lambda (_id entry)
              (kill-new (elt entry 1))
              (message "copied %S" (elt entry 1)))
    (setq tabulated-list-sort-key '("Alias" . nil))))

;;; Gitconfig-mode
(defun nvp-gitconfig-jump-to-alias (alias)
  "Jump to alias in .gitconfig file."
  (interactive
   (list (completing-read "Alias: " (nvp-git-aliases))))
  (goto-char (point-min))
  (search-forward "[alias]" nil t)
  (re-search-forward (concat "^\\s-*" (regexp-quote alias)) nil t))

;; -------------------------------------------------------------------
;;; Random Git

;; checkout part of a repo
;;;###autoload
(defun nvp-vc-git-sparse-clone (repo branch subdir local-dir)
  "Clone SUBDIR from REPO on BRANCH to LOCAL-DIR."
  (interactive
   (let* ((repo (read-string "Repository URI: "))
          (branch (read-string "Branch: " "master"))
          (subdir (let ((dir (read-string "Subdirectory to clone: ")))
                    (if (string-suffix-p "/*" dir) dir
                      (concat dir "/*"))))
          (ddir (car (last (split-string repo "/"))))
          (local-dir
           (read-directory-name "Clone into: " default-directory ddir nil ddir)))
     (list repo branch subdir local-dir)))
  (unless (file-exists-p local-dir)
    (mkdir local-dir))
  (let ((default-directory local-dir))
    (if (zerop (call-process-shell-command
                (concat "git init && git remote add -f origin " repo
                        " && git config core.sparseCheckout true")
                nil nil nil))
        (with-current-buffer (find-file-noselect ".git/info/sparse-checkout")
          (goto-char (point-max))
          (insert subdir)
          (pop-to-buffer-same-window (current-buffer))
          (add-hook 'after-save-hook
                    (lambda ()
                      (let ((default-directory local-dir))
                        (start-process "git" nil "git" "pull" "origin" branch)))
                    nil 'local))
      (user-error "Failed to configure repo for sparse checkout"))))

(provide 'nvp-vc)
;;; nvp-vc.el ends here
