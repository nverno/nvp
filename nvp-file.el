;;; nvp-file --- 

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
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
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib)
  (defvar recentf-list))

;; Open nearest file up the directory tree named:
;; 1. NAME if non-nil
;; 2. Prompt for file name with prefix arg
;; 3. Variable `notes-file' (directory-local) if non-nil
;; 4. Otherwise, default to 'todo.org'.
;;;###autoload
(defun nvp-file-open-nearest-dwim (name)
  (interactive
   (list (or
          (bound-and-true-p name)
          (and current-prefix-arg
               (read-from-minibuffer "File name: "))
          (bound-and-true-p notes-file)
          "todo.org")))
  (let ((dir (locate-dominating-file (or
                                      (buffer-file-name)
                                      default-directory)
                                     name)))
    (if dir (find-file (expand-file-name name dir))
      (user-error
       (format "%s not found up the directory tree." name)))))

;;--- recentf --------------------------------------------------------

;; Find a recent file using ido completion, trimming to basename.
;;;###autoload
(defun nvp-file-recentf-ido-find-file-trim ()
  (interactive)
  (if (not (bound-and-true-p recentf-mode))
      (recentf-mode))
  (let* ((file-assoc-list
	  (mapcar (lambda (x)
		    (cons (file-name-nondirectory x)
			  x))
		  recentf-list))
	 (filename-list
	  (cl-remove-duplicates (mapcar #'car file-assoc-list)
                                :test #'string=))
	 (filename (ido-completing-read "choose recent file: "
					filename-list
					nil
					t)))
    (when filename
      (find-file (cdr (assoc filename file-assoc-list))))))

;; Find a recent file using ido completion, only abbreviating
;; filenames.
;;;###autoload
(defun nvp-file-recentf-ido-find-file ()
  (interactive)
  (if (not (bound-and-true-p recentf-mode))
      (recentf-mode))
  (let* ((file-assoc-list
	  (mapcar (lambda (x)
	            (cons (abbreviate-file-name x) x))
	          recentf-list))
	 (filename-list
	  (cl-remove-duplicates (mapcar #'car file-assoc-list)
			     :test #'string=))
	 (filename (ido-completing-read "Recent File: "
					filename-list nil t)))
    (when filename
      (find-file (cdr (assoc filename file-assoc-list))))))

;;--- Directories ----------------------------------------------------

;; Subdirectories in `DIR'.
;;;###autoload
(defun nvp-file-subdirs (dir)
  (delq nil
        (mapcar
         (lambda (x)
           (let (y)
             (when (file-directory-p
                    (setq y (expand-file-name x dir)))
               (cons x y))))
         (cl-set-difference
          (directory-files dir)
          '("." "..")
          :test #'equal))))

;; Create directory when finding file requires.
;;;###autoload
(defun nvp-file-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p 
		(format "directory `%s' does not exist! create it?" 
			parent-directory)))
      (make-directory parent-directory t))))

;;--- Utils ----------------------------------------------------------

;; Add buffer file name or marked file to kill.
;;;###autoload
(defun nvp-file-copy-name ()
  (interactive)
  (if (eq major-mode 'dired-mode)
      (prog1
          (dired-copy-filename-as-kill 0)
        (message "Copied marked filenames."))
    (let ((filename (buffer-file-name)))
      (when filename
        (kill-new filename)
        (message "Copied: %s" filename)))))

;;;###autoload
(defun nvp-file-to-string (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;; Create file path from list of strings.
;;;###autoload
(defun nvp-file-path (args &optional sep)
  (mapconcat #'file-name-as-directory args (if sep sep "")))

;; Open FILENAME, load it into a buffer, generate the md5 of its
;; contents, and prepend to the kill-ring.
;;;###autoload
(defun nvp-file-md5 (filename)
  (interactive "f")
  (with-temp-buffer
    (insert-file-contents filename)
    (kill-new (md5 (current-buffer)))))

;; -------------------------------------------------------------------

(declare-function dired-copy-filename-as-kill "dired")

(provide 'nvp-file)
;;; nvp-file.el ends here
