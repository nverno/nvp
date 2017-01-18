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
;;; Util

;; possible test directories (choose first found):
;; - test tests t
(defun nvp-test--locate-tests ()
  (when-let ((root (nvp-project-locate-root)))
    (let* ((default-directory root)
           (dir (cl-find-if #'file-exists-p nvp-project--test-dir)))
      (and dir (expand-file-name dir)))))

;; list of elisp test files in project test folder
(defun nvp-test--test-files ()
  (when-let ((test (nvp-test--locate-tests)))
    (directory-files test t nvp-project--test-re)))

;; return test file if only one in directory, otherwise prompt with
;; completing read
(defun nvp-test-select-test ()
  (let ((files (nvp-test--test-files)))
    (and files
         (or (and (> (length files) 1)
                  (funcall-interactively 'ido-completing-read "Test file: " files))
             (car files)))))

(defmacro nvp-with-test (&optional no-test &rest body)
  "Do BODY in project test file, prompting if more than one is found.
Do NO-TEST if no tests are found, default to user-error."
  (declare (indent defun))
  `(let ((test-file (nvp-test-select-test)))
     (if (not test-file)
         ,(or no-test '(user-error "No test files found."))
       (with-current-buffer (find-file-noselect test-file)
         ,@body))))

;; -------------------------------------------------------------------
;;; Commands 

;; jump to project test file
;;;###autoload
(defun nvp-test-jump-to-test ()
  (interactive)
  (nvp-with-test nil (pop-to-buffer (current-buffer))))

(provide 'nvp-test)
;;; nvp-test.el ends here
