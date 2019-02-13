;;; nvp-find.el --- find stuff -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-02-13 14:07:16>
;; Package-Requires: 
;; Created: 24 November 2016

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
;; Autoloaded file commands
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib)
  (defvar recentf-list))
(require 'nvp)

;; Open nearest file up the directory tree named:
;; 1. NAME if non-nil
;; 2. Prompt for file name with prefix arg
;; 3. Variable `nvp-notes-local-file' (directory-local) if non-nil
;; 4. Otherwise, default to 'todo.org'.
;;;###autoload
(defun nvp-find-nearest-file-dwim (name)
  "Jump to nearest notes file, prompting with prefix."
  (interactive
   (list (or (and current-prefix-arg (read-file-name "File name: "))
             (bound-and-true-p nvp-notes-local-file)
             "todo.org")))
  (let ((dir (locate-dominating-file (or (buffer-file-name)
                                         default-directory)
                                     name)))
    (if dir (find-file (expand-file-name name dir))
      (user-error (format "%s not found up the directory tree." name)))))

;; -------------------------------------------------------------------
;;; Recentf 

;; Find a recent file using ido completion, only abbreviating
;; filenames.
;;;###autoload
(defun nvp-find-recentf ()
  (interactive)
  (if (not (bound-and-true-p recentf-mode))
      (recentf-mode))
  (let* ((file-assoc-list
	  (mapcar (lambda (x)
	            (cons (abbreviate-file-name x) x))
	          recentf-list))
	 (filename-list
	  (cl-remove-duplicates (mapcar #'car file-assoc-list) :test #'string=))
	 (filename (nvp-completing-read "Recent File: " filename-list nil t)))
    (when filename
      (find-file (cdr (assoc filename file-assoc-list))))))

(provide 'nvp-find)
;;; nvp-find.el ends here
