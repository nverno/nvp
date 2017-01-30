;;; nvp-project ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created:  2 December 2016

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib)
  (require 'subr-x))
(require 'projectile)

;; dynamic local variables
(defvar-local nvp-project--root '(".git" ".projectile" "test" "tests"))
(defvar-local nvp-project--test-re ".*tests?")
(defvar-local nvp-project--test-dir '("test" "tests" "t"))
(defvar-local nvp-project--test-fmt "test-%s")

;; -------------------------------------------------------------------
;;; Util

(cl-defmacro nvp-with-project ((&key (test-re nvp-project--test-re)
                                     (root nvp-project--root)
                                     (test-dir nvp-project--test-dir))
                               &rest body)
  (declare (indent defun) (debug t))
  `(let ((nvp-project--test-re ,test-re)
         (nvp-project--root (nvp-listify ,root))
         (nvp-project--test-dir ',test-dir))
     ,@body))

(defmacro nvp-with-project-root (&optional local &rest body)
  (declare (indent defun) (debug t))
  `(let ((default-directory (nvp-project-locate-root ,local)))
     ,@body))

(defun nvp-project-locate-root (&optional local)
  (if local (projectile-root-top-down
             (or buffer-file-name default-directory)
             (nvp-listify nvp-project--root))
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
     projectile-test-prefix-function
     projectile-test-suffix-function)
  "Register project type and create hook to set local variables."
  (declare (indent defun))
  (let ((hook (intern (concat "nvp-project-" (nvp-stringify type) "-setup"))))
   `(progn
      ,(and marker-files
            `(projectile-register-project-type
              ',type ,marker-files ,compile-cmd ,test-cmd ,run-cmd))

      (defun ,hook ()
        (setq-local nvp-project--root ',project-root)
        (setq-local nvp-project--test-dir ',test-dir)
        (setq-local nvp-project--test-re ,test-re)
        (setq-local nvp-project--test-fmt ,test-fmt)
        ,(and test-init-function
              `(setq-local nvp-test-init-function ,test-init-function))
        ,(and test-buffer-function
              `(setq-local nvp-test-init-buffer-function ,test-buffer-function))
        ,(and projectile-test-suffix-function
              `(setq-local ,projectile-test-suffix-function))
        ,(and projectile-test-prefix-function
              `(setq-local ,projectile-test-prefix-function))))))

;; -------------------------------------------------------------------
;;; Commands 

;;;###autoload
(defun nvp-project-projectile ()
  (interactive)
  (unless projectile-mode
    (projectile-mode))
  (global-set-key (kbd "<f2> p p") #'projectile-commander)
  (call-interactively 'projectile-commander))

;;;###autoload
(defun nvp-project-jump-to-git (arg)
  (interactive "P")
  (let ((name (nvp-project-name arg)))
    (and name (browse-url (concat "https://github.com/nverno/" name)))))

;;;###autoload
(defun nvp-project-jump-to-projectile ()
  (interactive)
  (nvp-with-project-root 'local
    (find-file-other-window ".projectile")))

(provide 'nvp-project)
;;; nvp-project.el ends here
