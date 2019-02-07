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

;; FIXME: relocate to `nvp-test'
(defvar-local nvp-project--test-re ".*tests?")
(defvar-local nvp-project--test-dir '("test" "tests" "t"))
(defvar-local nvp-project--test-fmt "test-%s")

;; -------------------------------------------------------------------
;;; Util

;; FIXME: make obsolete
(cl-defmacro nvp-with-project ((&key (test-re nvp-project--test-re)
                                     (root nvp-project--root)
                                     (test-dir nvp-project--test-dir))
                               &rest body)
  (declare (indent defun) (debug t))
  `(let ((nvp-project--test-re ,test-re)
         (nvp-project--root ',(nvp-listify `,root))
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

;; -------------------------------------------------------------------
;;; Commands 

;;;###autoload
(defun nvp-project-projectile ()
  "Load and remap keys for projectile."
  (interactive)
  (unless projectile-mode
    (projectile-mode))
  (global-set-key (kbd "<f2> p p") #'projectile-commander)
  (call-interactively #'projectile-commander))

;; -------------------------------------------------------------------
;;; Jumping to locations

;;;###autoload
(defun nvp-project-jump-to-gitpage (arg)
  "Jump to project's git page.
With prefix ARG 4 or 64 prompt for project name, with prefix 16 or 64 prompt 
for base URI."
  (interactive "P")
  (let ((uri (if (cl-member arg '((16) (64)) :test #'equal)
                 (read-string "URI: " "https://")
               "https://github.com/nverno/"))
        (name (nvp-project-name (cl-member arg '((4) (64)) :test #'equal))))
    (and (not (string-suffix-p "/" uri)) (setq uri (concat uri "/")))
    (and name (browse-url (concat uri name)))))

;;;###autoload
(defun nvp-project-jump-to-projectile ()
  "Jump to project's projectile file."
  (interactive)
  (nvp-with-project-root :local t
    (find-file-other-window ".projectile")))

;;;###autoload
(defun nvp-project-jump-to-notes ()
  "Jump to project's notes.org file."
  (interactive)
  (nvp-with-project-root :local t
    (find-file-other-window "notes.org")))

;;;###autoload
(defun nvp-project-jump-to-makefile (&optional root)
  "Jump to closest Makefile.
With \\[universal-argument] jump to root Makefile."
  (interactive "P")
  (if (not root)
      (find-file-other-window (locate-dominating-file ))
      (nvp-with-project-root
        
        (find-file-other-window ))))

(provide 'nvp-project)
;;; nvp-project.el ends here
