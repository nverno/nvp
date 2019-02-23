;;; nvp-file.el --- File helpers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-22 21:51:05>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created:  6 February 2019

;;; Commentary:
;; file utils
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'nvp-macro))

;; -------------------------------------------------------------------
;;; Utils

(defun nvp-file-locate-first-dominating (file names)
  "Locate first name in NAMES using `locate-dominating-file' starting from FILE."
  (cl-loop for name in names
     as res = (locate-dominating-file file name)
     when res
     return res))

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
  (let ((coding-system-for-write 'binary)
        (write-region-annotate-functions nil)
        (write-region-post-annotation-function nil))
    (write-region data nil file append 'silent)
    nil))

(defun nvp-file-append-bytes (data file)
  "Append binary DATA to FILE.
FILE is created if it doesn't exist."
  (nvp-file-write-bytes data file 'append))

(cl-defun nvp-file-append (text file &optional (coding 'utf-8))
  "Append TEXT to FILE with CODING."
  (nvp-file-append-bytes (encode-coding-string text coding) file))

(cl-defun nvp-file-write (text file &optional (coding 'utf-8))
  "Write TEXT to FILE with CODING."
  (nvp-file-write-bytes (encode-coding-string text coding) file))

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
