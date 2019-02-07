;;; nvp-file.el --- File helpers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-06 20:30:36>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Maintainer: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created:  6 February 2019

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
  (require 'cl-lib)
  (require 'subr-x)
  (require 'nvp-macro))

;; -------------------------------------------------------------------
;;; Utils


(defun nvp-file-create-path (args &optional sep)
  "Create file path from list of ARGS (strings) components."
  (mapconcat #'file-name-as-directory args (if sep sep "")))

(defun nvp-file-md5 (filename)
  "Generate MD5 of FILENAME contents and prepend to `kill-ring'."
  (interactive "f")
  (with-temp-buffer
    (insert-file-contents filename)
    (kill-new (md5 (current-buffer)))))

;;; I/O
;; see f.el
(defun nvp-file-bytes (file)
  "Return FILE contents as bytes returning unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (setq buffer-file-coding-system 'binary)
    (insert-file-contents-literally file)
    (buffer-string)))

(cl-defun nvp-file-string (file &optional (coding 'utf-8))
  "Return FILE contents as string with default CODING utf-8."
  (decode-coding-string (nvp-file-bytes file) coding))

;; see f.el `f--write-bytes'
(defun nvp-file-write-bytes (data file &optional append)
  "Write binary DATA to FILE.
If APPEND is non-nil, append DATA to existing contents."
  (when (not (multibyte-string-p data))
    (signal 'wrong-type-argument (list 'multibyte-string-p data)))
  (let ((coding-system-for-write 'binary))))

;; -------------------------------------------------------------------
;;; Directories 

(defun nvp-file-subdirs (dir)
  "Retutn alist of (dir-name . full-path) for subdirectories of DIR."
  (delq nil
        (mapcar
         (lambda (x)
           (let (y)
             (when (file-directory-p
                    (setq y (expand-file-name x dir)))
               (cons x y))))
         (cl-set-difference (directory-files dir) '("." "..") :test #'equal))))

(provide 'nvp-file)
;;; nvp-file.el ends here
