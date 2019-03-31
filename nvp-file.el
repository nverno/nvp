;;; nvp-file.el --- File helpers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-31 10:00:59>
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

(defsubst nvp-file-owner-uid (file)
  (nth 2 (file-attributes file 'integer)))

(defsubst nvp-file-owned-by-user-p (file)
  (equal (nvp-file-owner-uid file) (user-uid)))

(defun nvp-already-root-p ()
  (let ((remote-method (file-remote-p default-directory 'method))
        (remote-user (file-remote-p default-directory 'user)))
    (and remote-method
         (or (member remote-method '("sudo" "su" "ksu" "doas"))
             (string= remote-user "root")))))

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

;; -------------------------------------------------------------------
;;; Commands

(defun nvp-file-md5 (filename)
  "Generate MD5 of FILENAME contents and prepend to `kill-ring'."
  (interactive "f")
  (with-temp-buffer
    (insert-file-contents filename)
    (kill-new (md5 (current-buffer)))))

;;-- sudo edit
(eval-when-compile
 (defmacro nvp-sudo-wrap (func &optional filename)
   `(let ((remote-method (file-remote-p default-directory 'method))
          (remote-host (file-remote-p default-directory 'host))
          (remote-localname (file-remote-p default-directory 'localname)))
      (,func (format "/%s:root:@%s:%s"
                     (or remote-method "sudo")
                     (or remote-host "localhost")
                     (or remote-localname
                         ,(or filename '(read-file-name "Find file (root): "))))))))

;; https://github.com/bbatsov/crux/blob/master/crux.el
(defun nvp-find-alternate-file-as-root (file)
  "Wrap `find-alternate-file' to open FILE as root."
  (nvp-sudo-wrap find-alternate-file file))

;;;###autoload
(defun nvp-file-sudo-edit (&optional arg)
  "Edit current file as root.
With prefix ARG, prompt for file to visit."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (nvp-sudo-wrap find-file)
    (if (nvp-already-root-p)
        (message "Already editing file as root.")
      (let ((place (point)))
        (nvp-find-alternate-file-as-root buffer-file-name)
        (goto-char place)))))

(provide 'nvp-file)
;;; nvp-file.el ends here
