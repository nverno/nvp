;;; nvp-file.el --- File helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; file utils
;;; Code:
(eval-when-compile
  (require 'nvp-macro))
(nvp-decls)

;; -------------------------------------------------------------------
;;; General file utils

(defun nvp-file-locate-first-dominating (file names)
  "Locate first name in NAMES using `locate-dominating-file' starting from FILE."
  (cl-loop for name in names
     as res = (locate-dominating-file file name)
     when res
     return res))

;; Create file path from list of ARGS (strings) components.
(defsubst nvp-file-create-path (args &optional sep)
  (mapconcat #'file-name-as-directory args (if sep sep "")))

(defsubst nvp-file-owner-uid (file)
  (nth 2 (file-attributes file 'integer)))

(defsubst nvp-file-owned-by-user-p (file)
  (equal (nvp-file-owner-uid file) (user-uid)))


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
;;; Remote

;; use if wanting to read history file over tramp connection
(defun nvp-create-remote-filename (filename)
  (if (file-remote-p default-directory)
      (tramp-make-tramp-file-name
       (tramp-dissect-file-name default-directory) filename)
    filename))


;; -------------------------------------------------------------------
;;; Sudo edit

(defun nvp-already-root-p ()
  (let ((remote-method (file-remote-p default-directory 'method))
        (remote-user (file-remote-p default-directory 'user)))
    (and remote-method
         (or (member remote-method '("sudo" "su" "ksu" "doas"))
             (string= remote-user "root")))))

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


;; -------------------------------------------------------------------
;;; Assorted commands

(defun nvp-file-md5 (filename)
  "Generate MD5 of FILENAME contents and prepend to `kill-ring'."
  (interactive "f")
  (with-temp-buffer
    (insert-file-contents filename)
    (kill-new (md5 (current-buffer)))))

(provide 'nvp-file)
;;; nvp-file.el ends here
