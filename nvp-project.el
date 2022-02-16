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
(defun nvp-project-root (&optional path)
  "Try `nvp-project-root-function' if it is defined.
Otherwise, look for version control directories, returing the longest path."
  (or path (setq path (buffer-file-name)))
  (if nvp-project-root-function
      (funcall nvp-project-root-function)
    (or (nvp:longest-item
         (vc-git-root path)
         (vc-svn-root path)
         (vc-hg-root path)))))

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

(defun nvp-project-invalidate-cmds ()
  "Reset cached projectile project commands."
  (interactive)
  (--when-let (projectile-acquire-root)
    (remhash it projectile-compilation-cmd-map)
    (remhash it projectile-configure-cmd-map)
    (remhash it projectile-install-cmd-map)
    (remhash it projectile-package-cmd-map)
    (remhash it projectile-test-cmd-map)
    (remhash it projectile-run-cmd-map)))

;; -------------------------------------------------------------------
;;; Test defining projectile project type

;; (defun nvp-projectile-related/elisp (path)
;;   (let ((name (nvp:bfn-no-ext path)))))
;; (projectile-register-project-type 'elisp '("Makefile" "Cask")
;;                                   :compile "make"
;;                                   :test-suffix "-test"
;;                                   :related-files-fn)

;;; FIXME: remove everything below here
;; -------------------------------------------------------------------
;;; Util

;; dynamic local variables
(defvar-local nvp-project--root '(".git" ".projectile" "test" "tests"))

;; FIXME: relocate to `nvp-test'
(defvar-local nvp-project--test-re ".*tests?")
(defvar-local nvp-project--test-dir '("test" "tests" "t"))
(defvar-local nvp-project--test-fmt "test-%s")

;; FIXME: make obsolete
(cl-defmacro nvp-with-project ((&key (test-re nvp-project--test-re)
                                     (root nvp-project--root)
                                     (test-dir nvp-project--test-dir))
                               &rest body)
  (declare (indent defun) (debug t))
  `(let ((nvp-project--test-re ,test-re)
         (nvp-project--root ',(nvp:listify `,root))
         (nvp-project--test-dir ',test-dir))
     ,@body))

(cl-defmacro nvp-with-project-root (&rest body &key local &allow-other-keys)
  "Execute BODY with \\[default-directory] bound to project root.
If LOCAL is non-nil find closest root."
  (declare (indent defun) (debug t))
  (while (keywordp (car body))
    (setq body (cdr (cdr body))))
  `(let ((default-directory (nvp-project-locate-root ,local)))
     ,@body))

(defun nvp-project-locate-root (&optional local)
  (if local (projectile-root-top-down
             (or buffer-file-name default-directory)
             (nvp:listify nvp-project--root))
    (projectile-project-root)))

(defun nvp-project-name (&optional arg)
  (let* ((project (or arg (nvp-project-locate-root)))
         (name (if arg
                   (read-from-minibuffer "Project name: ")
                 (if project
                     (file-name-nondirectory
                      (directory-file-name project))
                   (user-error "No project found.")))))
    (or name (nvp-project-name t))))

(cl-defmacro nvp-define-project
    (type
     &key
     (project-root nvp-project--root)
     ;; projectile stuff
     marker-files
     compile-cmd
     test-cmd
     run-cmd
     ;; tests
     (test-dir nvp-project--test-dir)
     (test-re nvp-project--test-re)
     (test-fmt nvp-project--test-fmt)
     test-init-function
     test-buffer-function
     test-run-unit-function
     projectile-test-prefix-function
     projectile-test-suffix-function)
  "Register project type and create hook to set local variables."
  (declare (indent defun))
  (let ((hook (intern (concat "nvp-project-" (symbol-name type) "-setup"))))
   `(progn
      ,(and marker-files
            `(projectile-register-project-type
              ',type ,marker-files ,compile-cmd ,test-cmd ,run-cmd))

      (defun ,hook ()
        (setq-local nvp-project--root ',project-root)
        ;; tests
        (setq-local nvp-project--test-dir ',test-dir)
        (setq-local nvp-project--test-re ,test-re)
        (setq-local nvp-project--test-fmt ,test-fmt)
        ,(and test-init-function
              `(setq-local nvp-test-init-function ,test-init-function))
        ,(and test-buffer-function
              `(setq-local nvp-test-init-buffer-function ,test-buffer-function))
        ,(and test-run-unit-function
              `(setq-local nvp-test-run-unit-function ,test-run-unit-function))
        ;; projectile tests
        ,(and projectile-test-suffix-function
              `(setq-local ,projectile-test-suffix-function))
        ,(and projectile-test-prefix-function
              `(setq-local ,projectile-test-prefix-function))))))


(provide 'nvp-project)
;;; nvp-project.el ends here
