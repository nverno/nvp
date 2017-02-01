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

;; called when visiting new test buffer, passed name of matching source file
(defvar nvp-test-init-function 'ignore)

;; called when visiting test buffer with no arguments
(defvar nvp-test-init-buffer-function 'ignore)

;; function to run unit file. Passed current buffer-file-name
(defvar nvp-test-run-unit-function 'ignore)

;; filter test files
(defvar nvp-test-prefixes '("test-" "test_" "t-" "t_" "Test"))
(defvar nvp-test-suffixes '("-test" "_test"))
(defvar nvp-test-extension-re nil)

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
  (if create
      (nvp-test-create-test-dir
       local (car (or test-dirs nvp-project--test-dir)))
    (when-let ((root (nvp-project-locate-root local)))
      (let* ((default-directory root)
             (test-dirs (or test-dirs nvp-project--test-dir))
             (dir (cl-find-if #'file-exists-p test-dirs)))
        (and dir (expand-file-name dir))))))

;; list of test files in project test folder
(defun nvp-test--test-files (dir &optional test-re)
  (directory-files dir t (or test-re nvp-project--test-re)))

;; provide a default name for either a new test or history of buffer-local
;; previous test files
(defvar-local nvp-test-file-history nil)
(defsubst nvp-test-default-test-name (file)
  (or (car-safe nvp-test-file-history)
      (format (or (bound-and-true-p nvp-project--test-fmt) "test-%s")
              (file-name-nondirectory file))))

;; If test file found that matches buffer-file-name, return that,
;; otherwise prompt with completing-read 
(defun nvp-test-find-matching-test (file test-dir &optional prefixes suffixes)
  (let* ((default-directory test-dir)
         (file-ext (or nvp-test-extension-re (file-name-extension file)))
         (files (directory-files
                 test-dir t (concat "^[^.].*" (regexp-quote file-ext) "$")))
         (basename (file-name-nondirectory (file-name-sans-extension file)))
         (test-file
          (or 
           ;; try to find test matching current buffer's name with test
           ;; prefix/suffixes
           (cl-find-if
            (lambda (f)
              (cl-member
               (file-name-sans-extension (file-name-nondirectory f))
               (append
                (mapcar (lambda (prefix) (concat prefix basename))
                        (or prefixes nvp-test-prefixes))
                (mapcar (lambda (suffix) (concat basename suffix))
                        (or suffixes nvp-test-suffixes)))
               :test 'string=))
            files)
           ;; read name of new test
           (expand-file-name
            (funcall-interactively
             'ido-completing-read "Name of (new) test file: "
             (mapcar 'file-name-nondirectory files)
             nil nil (nvp-test-default-test-name file))
            default-directory))))
    ;; keep history
    (cl-pushnew (file-name-nondirectory test-file) nvp-test-file-history)
    test-file))

(defmacro nvp-with-test (&optional local create no-test prefixes suffixes &rest body)
  "Do BODY in project test file, prompting if more than one is found.
Do NO-TEST if no tests are found, default to user-error."
  (declare (indent defun))
  `(let ((test-dir (nvp-test-dir ,local ,create)))
     (unless test-dir
       (user-error "No test directory found."))
     (let* ((source-file (buffer-file-name))
            (test-file
             (nvp-test-find-matching-test source-file test-dir ,prefixes ,suffixes))
            (new-file (not (file-exists-p test-file)))
            (init-function nvp-test-init-function)
            (buffer-function nvp-test-init-buffer-function))
       (if (not test-file)
           ,(or no-test '(user-error "No test files found."))
         (with-current-buffer (find-file-noselect test-file)
           (and new-file (funcall-interactively init-function source-file))
           (funcall-interactively buffer-function)
           ,@body)))))

;; -------------------------------------------------------------------
;;; Commands 

;; jump to test file associated with current source file
;;;###autoload
(defun nvp-test-jump-to-test (arg)
  (interactive "P")
  (nvp-with-test 'local arg nil nil nil
    (pop-to-buffer (current-buffer))))

;; Run or create test matching current source file
;;;###autoload
(defun nvp-test-run-matching-test (create)
  (interactive "P")
  (let ((test-file (nvp-test-find-matching-test (buffer-file-name)
                                                (nvp-test-dir 'local create))))
    (funcall-interactively nvp-test-run-unit-function test-file)))

(provide 'nvp-test)
;;; nvp-test.el ends here
