;;; nvp-session.el --- persistent session management -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Stores lightweight named sessions managed through desktop.el
;; Extends to support:
;; - multiple sessions
;; - save / load / switch / delete
;; - a basic menu to display and manage them
;;
;;; Code:

(eval-when-compile (require 'nvp-macro))
(require 'desktop)
(nvp:decls :f (nvp-buffer-kill-all-buffers))

(defsubst nvp--sessions (&optional full)
  (directory-files (expand-file-name "sessions" nvp/cache) full "^.[^.]"))

(defsubst nvp--session-read (&optional prompt)
  "Read session and return list of \\='(filename session-name)."
  (list (expand-file-name "sessions" nvp/cache)
        (completing-read (or prompt "Session: ") (nvp--sessions))))

(defmacro with-desktop-vars (dirname filename &rest body)
  (declare (indent defun))
  `(cl-letf (((symbol-function #'desktop-owner) #'ignore)
             (desktop-base-file-name (or ,filename ".emacs.desktop"))
             (desktop-dirname ,dirname))
     ,@body))

;;;###autoload
(defun nvp-session-save (&optional dirname name)
  "Read a session and save it."
  (interactive (nvp--session-read "Save session: "))
  (with-desktop-vars dirname name
    (desktop-save dirname)))

;;;###autoload
(defun nvp-session-load (&optional dirname name)
  "Read a session and load it."
  (interactive (nvp--session-read "Load session: "))
  (call-interactively #'nvp-buffer-kill-all-buffers)
  (with-desktop-vars dirname name
    (desktop-read dirname)))

;;;###autoload
(defun nvp-session-delete (&optional dirname name)
  "Read a session and deleete it."
  (interactive (nvp--session-read "Delete session: "))
  (delete-file (expand-file-name name dirname)))


;;; Session List

(defvar-keymap nvp-sessions-mode-map
  "d"   #'nvp-sessions-list-delete
  "o"   #'nvp-sessions-list-load
  "l"   #'nvp-sessions-list-load
  "r"   #'nvp-sessions-list-rename
  "RET" #'nvp-sessions-list-load)

(defun nvp-sessions-tabulate ()
  (cl-loop for session in (nvp--sessions 'full) collect
       (let* ((attr (file-attributes session 'integer))
              (mod-time (format-time-string
                         "%D" (file-attribute-access-time attr)))
              (access-time (format-time-string
                            "%D" (file-attribute-modification-time attr))))
         `(,session [,(file-name-nondirectory session)
                     ,access-time ,mod-time]))))

(define-derived-mode nvp-sessions-mode tabulated-list-mode "Sessions"
  "Mode for listing persistent sessions."
  (setq tabulated-list-format [("Name" 25 t)
                               ("Last Access" 12 t)
                               ("Modtime" 12 t)])
  (setq tabulated-list-entries #'nvp-sessions-tabulate)
  (setq tabulated-list-sort-key '("Last Access" . t))
  (tabulated-list-init-header)
  (tabulated-list-print))

;;;###autoload
(defun nvp-sessions-list ()
  "List saved sessions."
  (interactive)
  (pop-to-buffer (get-buffer-create "*sessions*"))
  (nvp-sessions-mode)
  (tabulated-list-print))

(defun nvp-sessions-list-delete ()
  "Delete session at point."
  (interactive)
  (let ((session (tabulated-list-get-id)))
    (when (and session (y-or-n-p "Delete session? "))
      (delete-file (expand-file-name (tabulated-list-get-id)))))
  (revert-buffer))

(defun nvp-sessions-list-load ()
  "Kill everything and load session at point."
  (interactive)
  (let ((session (tabulated-list-get-id)))
    (when (and session (y-or-n-p "Load session? "))
      (nvp-session-load (file-name-directory session)
                        (file-name-nondirectory session)))))

(defun nvp-sessions-list-rename (name)
  "Rename sesssion at point."
  (interactive "sRename session: ")
  (let ((session (tabulated-list-get-id)))
    (rename-file session (expand-file-name
                          name (file-name-directory session)))
    (revert-buffer)))

(provide 'nvp-session)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-session.el ends here
