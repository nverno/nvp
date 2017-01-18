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

;; dynamic local variables
(defvar-local nvp-project--test-re ".*tests?")
(defvar-local nvp-project--root ".git")
(defvar-local nvp-project--test-dir '("test" "tests" "t"))

;; -------------------------------------------------------------------
;;; Util

(cl-defmacro nvp-with-project ((&key (test-re nvp-project--test-re)
                                     (root nvp-project--root)
                                     (test-dir nvp-project--test-dir))
                               &rest body)
  (declare (indent defun) (debug t))
  `(let ((nvp-project--test-re ,test-re)
         (nvp-project--root ,root)
         (nvp-project--test-dir ',test-dir))
     ,@body))

(defun nvp-project-locate-root ()
  (locate-dominating-file
   (or buffer-file-name default-directory) nvp-project--root))

(defun nvp-project-name (&optional arg)
  (let* ((project (or arg (nvp-project-locate-root)))
         (name (if arg
                   (read-from-minibuffer "Project name: ")
                 (if project
                     (file-name-nondirectory
                      (directory-file-name project))
                   (user-error "No project found.")))))
    (or name (nvp-project-name t))))

;; -------------------------------------------------------------------
;;; Commands 

;;;###autoload
(defun nvp-project-jump-to-git (arg)
  (interactive "P")
  (let ((name (nvp-project-name arg)))
    (and name (browse-url (concat "https://github.com/nverno/"
                                  name)))))

(provide 'nvp-project)
;;; nvp-project.el ends here
