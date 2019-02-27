(require 'eproject)
(declare-function magit-run-git "magit-process")

(defvar perl-tools-do-not-want-perl-project nil
  "Remember `n' answers to 'do you want a project?'.")

(defun perl-tools-already-have-perl-project-p (root)
  (or (string-equal (ignore-errors (eproject-root)) root)
      (or (member root perl-tools-do-not-want-perl-project))
      (and (file-exists-p (concat root ".git"))
           (or (file-exists-p (concat root "dist.ini"))
               (file-exists-p (concat root "Makefile.PL"))
               (file-exists-p (concat root "Build.PL"))))))

(defun perl-tools-maybe-init-perl-project ()
  "Create a git + cpan project for the file visited by the current buffer."
  (let* ((filename (buffer-file-name))
         ;; seeing lib/ ensures that this is really
         ;; a project and not a one-off thing
         (root (and (string-match "^\\(.+/\\)lib/.+[.]pm$" filename)
                    (match-string 1 filename))))
  (when (and root (not (perl-tools-already-have-perl-project-p root)))
    (if (y-or-n-p "Create a new project here? ")
        (perl-tools-init-perl-project root)
      (when (not (member root perl-tools-do-not-want-perl-project))
        (setq perl-tools-do-not-want-perl-project (cons root perl-tools-do-not-want-perl-project)))))))

(defun perl-tools-init-perl-project (root)
  (interactive "DRoot: ")
  (let ((default-directory root)
        (project-name (file-name-nondirectory (directory-file-name root))))
    (when (not (file-exists-p (concat root ".git")))
      (magit-init root))
    (perl-tools-project-starter-build-gitignore :root root :name project-name)
    (perl-tools-project-starter-build-dist.ini :root root :name project-name)
    (perl-tools-project-starter-build-Changes :root root :name project-name)
    (perl-tools-project-starter-build-eproject :root root)

    (magit-run-git "commit" "-m" "project boilerplate added")

    (ignore-errors (eproject-reinitialize-project))
    (eproject-maybe-turn-on)))

(defun perl-tools-project-starter-git-init (root)
  (shell-command (format "cd %s; git init" root)))

(cl-defmacro perl-tools-project-starter-make-file ((root file) &body forms)
  (declare (indent 1))
  `(when (not (file-exists-p (concat ,root ,file)))
     (with-temp-buffer
       ,@forms
       (write-file (concat ,root ,file))
       (magit-run-git "add" (concat ,root ,file)))))

(cl-defun perl-tools-project-starter-build-dist.ini (&key root name)
  (perl-tools-project-starter-make-file (root "dist.ini")
    (insert (format "name = %s\n[@JROCKWAY]\n" name))))

(cl-defun perl-tools-project-starter-build-gitignore (&key root name)
  (perl-tools-project-starter-make-file (root ".gitignore")
    (insert (format "cover_db\nTAGS\n%s*\n.build\n" name))))

(cl-defun perl-tools-project-starter-build-Changes (&key root name)
  (perl-tools-project-starter-make-file (root "Changes")
    (insert (format "Change history for %s\n{{$NEXT}}\n" name))))

(cl-defun perl-tools-project-starter-build-eproject (&key root)
  (perl-tools-project-starter-make-file (root ".eproject")
    (insert ":mxdeclare-project-p nil\n")))

(provide 'perl-project-starter)

;;; perl-project-starter.el ends here
