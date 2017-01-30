;;; nvp-test ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 18 January 2017

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
(require 'nvp-project)

;; -------------------------------------------------------------------
;;; Find test files 

;; create test directory in project root or local root if doesn't exist
(defun nvp-test-create-test-dir (&optional local dir)
  (let ((test-dir (expand-file-name
                   (or dir "test") (nvp-project-locate-root local))))
    (unless (file-exists-p test-dir)
      (make-directory test-dir 'create-parents))
    test-dir))

;; possible test directories (choose first found):
;; - test tests t
(defun nvp-test-dir (&optional local create test-dirs)
  (when-let ((root (nvp-project-locate-root local)))
    (let* ((default-directory root)
           (test-dirs (or test-dirs nvp-project--test-dir))
           (dir (cl-find-if #'file-exists-p test-dirs)))
      (if dir (expand-file-name dir)
        (and create test-dirs
             (nvp-test-create-test-dir local (car test-dirs)))))))

;; list of elisp test files in project test folder
(defun nvp-test--test-files (&optional local create test-dirs test-re)
  (when-let ((test (nvp-test-dir local create test-dirs)))
    (directory-files test t (or test-re nvp-project--test-re))))

;; If test file found that matches buffer-file-name, return that,
;; otherwise prompt with completing-read 
(defun nvp-test-find-test (file &optional local create prefixes suffixes
                                test-dirs test-re)
  (let ((files (nvp-test--test-files local create test-dirs test-re))
        (basename (file-name-nondirectory (file-name-sans-extension file))))
    (when files
      (or 
       (cl-find-if
        (lambda (f)
          (cl-member
           (file-name-sans-extension (file-name-nondirectory f))
           (append
            (mapcar (lambda (prefix) (concat prefix basename))
                    (or prefixes '("t-" "t_" "test-" "test_")))
            (mapcar (lambda (suffix) (concat basename suffix))
                    (or suffixes '("-test" "_test"))))
           :test 'string=))
        files)
       (let ((test-dir (file-name-directory (car files))))
         (expand-file-name
          (funcall-interactively 'ido-completing-read "Test file: "
                                 (mapcar 'file-name-nondirectory files))
          test-dir))))))

(defmacro nvp-with-test (&optional local create no-test prefixes suffixes
                                   &rest body)
  "Do BODY in project test file, prompting if more than one is found.
Do NO-TEST if no tests are found, default to user-error."
  (declare (indent defun))
  `(let ((test-file (nvp-test-find-test (buffer-file-name) ,local ,create
                                        ,prefixes ,suffixes)))
     (if (not test-file)
         ,(or no-test '(user-error "No test files found."))
       (with-current-buffer (find-file-noselect test-file)
         ,@body))))

;; -------------------------------------------------------------------
;;; Commands 

;; jump to project test file
;;;###autoload
(defun nvp-test-jump-to-test (arg)
  (interactive "P")
  (nvp-with-test 'local arg nil nil nil (pop-to-buffer (current-buffer))))

(provide 'nvp-test)
;;; nvp-test.el ends here
