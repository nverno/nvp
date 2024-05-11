;;; nvp-project.el --- project management -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Frameworks:
;; - project.el (builtin -- wip)
;; - projectile.el
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'projectile)
(nvp:decls)
(nvp:auto "vc-git" 'vc-git-root)
(nvp:auto "vc-hg" 'vc-hg-root)
(nvp:auto "vc-svn" 'vc-svn-root)


;;;###autoload
(defun nvp-project-locate-root (&optional dir local roots)
  (if local (projectile-root-top-down
             (or dir buffer-file-name default-directory)
             (and roots (nvp:listify roots)))
    (projectile-project-root dir)))

;;;###autoload
(defun nvp-project-root (&optional path local)
  "Try `nvp-project-root-function' if it is defined.
Otherwise, look for version control directories, returing the longest path."
  (or path (setq path (or (buffer-file-name) default-directory)))
  (if nvp-project-root-function
      (funcall nvp-project-root-function path local)
    (let ((fn (if local
                  (lambda (a b) (if (< (length a) (length b)) a b))
                (lambda (a b) (if (> (length a) (length b)) a b)))))
      (cl-reduce fn (list (vc-git-root path)
                          (vc-svn-root path)
                          (vc-hg-root path))))))

;;;###autoload
(defun nvp-project-parent (&optional maxdepth directory)
  "Return project root containing current project, or MAXDEPTH levels up."
  (cl-assert (or (null maxdepth) (>= maxdepth 0)))
  (or maxdepth (setq maxdepth 100))
  (let ((default-directory (or directory default-directory)) res cur)
    (while (and (or (null maxdepth) (>= maxdepth 0))
                (setq cur (funcall nvp-project-root-function)))
      (setq res cur
            default-directory (nvp:parent cur)
            maxdepth (1- maxdepth)))
    res))

;;; TODO: display more info: project's runner commands
;;;###autoload
(defun nvp-project-info ()
  "Display info for current project."
  (interactive)
  (if (projectile-project-p)
      (projectile-project-info)
    (message "Not in a known project.")))

;;;###autoload
(defun nvp-project-invalidate-cmds ()
  "Reset cached projectile project commands."
  (interactive)
  (-when-let (project (projectile-acquire-root))
    (--each '(projectile-compilation-cmd-map
              projectile-configure-cmd-map
              projectile-install-cmd-map
              projectile-package-cmd-map
              projectile-test-cmd-map
              projectile-run-cmd-map)
      (ignore-errors (remhash project it)))))

(defun nvp-project-name (&optional arg)
  (let* ((project (or arg (nvp-project-locate-root)))
         (name (if arg
                   (read-from-minibuffer "Project name: ")
                 (if project
                     (file-name-nondirectory
                      (directory-file-name project))
                   (user-error "No project found.")))))
    (or name (nvp-project-name t))))

(provide 'nvp-project)
;;; nvp-project.el ends here
