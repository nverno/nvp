(nvp:auto "nvp-util" 'nvp-s-all-matches)

;; -------------------------------------------------------------------
;;; SVN 

;;; NOTE: if want to use this again, the cache needs to be created
;; synchronously (for the completion reading) or, use a different
;; caching technique

;; Cached list of git svn subcommands
(nvp:define-cache nvp-vc-svn--available-commands ()
  "List of git svn subcommands."
  (nvp:with-process "git"
    :proc-name "git"
    :proc-args ("svn" "help")
    :on-success (setq nvp-vc-svn--available-commands
                      (nvp-s-all-matches "^  \\([-a-z]+\\) +" (buffer-string) 1))
    :shell t))

;;;###autoload
(defun nvp-vc-svn (dir command)
  "Run a git svn subcommand in `DIR'."
  (interactive (list (read-directory-name "Directory: " nil default-directory)
                     (nvp-completing-read
                      "Git svn command to run: "
                      (nvp-vc-svn--available-commands)
                      nil t nil nil)))
  (let* ((default-directory (vc-git-root dir))
         (compilation-buffer-name-function
          #'(lambda (_major-mode-name) "*git-svn*")))
    (compile (concat "git svn " command))))

