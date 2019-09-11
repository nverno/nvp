;;; nvp-session.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'desktop)
(nvp-declare nvp-buffer-kill-all-buffers)

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
