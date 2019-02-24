;;; nvp-session.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-24 04:52:26>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 30 March 2017

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (nvp-local-vars)
  (require 'cl-lib))
(require 'nvp)
(require 'desktop)
(nvp-declare "nvp-buffer" nvp-buffer-kill-all-buffers)

(defsubst nvp-session--read (prompt)
  (list
   (expand-file-name "sessions" nvp/cache)
   (nvp-completing-read
    prompt
    (directory-files (expand-file-name "sessions" nvp/cache) nil "^.[^.]"))))

(eval-when-compile
  (defmacro with-desktop-vars (dirname filename &rest body)
   (declare (indent defun))
   `(let ((desktop-base-file-name (or ,filename ".emacs.desktop"))
          (desktop-dirname ,dirname))
      ,@body)))

;;;###autoload
(defun nvp-session-save (&optional dirname name)
  (interactive (nvp-session--read "Save session: "))
  (with-desktop-vars dirname name
    (desktop-save dirname)))

;;;###autoload
(defun nvp-session-load (&optional dirname name)
  (interactive (nvp-session--read "Load session: "))
  (call-interactively #'nvp-buffer-kill-all-buffers)
  (with-desktop-vars dirname name
    (desktop-read dirname)))

;;;###autoload
(defun nvp-session-delete (&optional dirname name)
  (interactive (nvp-session--read "Delete session: "))
  (delete-file (expand-file-name name dirname)))

(provide 'nvp-session)
;;; nvp-session.el ends here
