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
(require 'nvp)
(require 'desktop)
(nvp:decls :f (nvp-buffer-kill-all-buffers))

(eval-when-compile
  (defsubst nvp-sessions (&optional full)
    (directory-files (expand-file-name "sessions" nvp/cache) full "^.[^.]"))
  
  (defsubst nvp-session--read (prompt)
    (list (expand-file-name "sessions" nvp/cache)
          (nvp-completing-read prompt (nvp-sessions))))

  (defmacro with-desktop-vars (dirname filename &rest body)
    (declare (indent defun))
    `(cl-letf (((symbol-function #'desktop-owner) #'ignore)
               (desktop-base-file-name (or ,filename ".emacs.desktop"))
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

;; -------------------------------------------------------------------
;;; List sessions

(defun nvp-sessions-tabulate ()
  (cl-loop for session in (nvp-sessions 'full) collect
       (let* ((attr (file-attributes session 'integer))
              (mod-time (format-time-string "%D" (file-attribute-access-time attr)))
              (access-time (format-time-string
                            "%D" (file-attribute-modification-time attr))))
         `(,session [,(file-name-nondirectory session) ,access-time ,mod-time]))))

(define-derived-mode nvp-sessions-mode tabulated-list-mode "Sessions"
  "Mode for listing persistent sessions."
  (setq tabulated-list-format [("Name" 25 t)
                               ("Last Access" 12 t)
                               ("Modtime" 12 t)])
  (setq tabulated-list-entries #'nvp-sessions-tabulate)
  (tabulated-list-init-header))

;;;###autoload
(defun nvp-sessions-list ()
  (interactive)
  (pop-to-buffer (get-buffer-create "*sessions*"))
  (nvp-sessions-mode)
  (tabulated-list-print))

(defun nvp-sessions-list-delete ()
  (interactive)
  (delete-file (expand-file-name (tabulated-list-get-id)))
  (revert-buffer))

(defun nvp-sessions-list-load ()
  (interactive)
  (let ((session (tabulated-list-get-id)))
    (nvp-session-load (file-name-directory session)
                      (file-name-nondirectory session))))

(defun nvp-sessions-list-rename (name)
  (interactive "sRename session: ")
  (let ((session (tabulated-list-get-id)))
    (rename-file session (expand-file-name name (file-name-directory session)))
    (revert-buffer)))

(nvp:bindings nvp-sessions-mode :now
  :create t
  ("d"   . nvp-sessions-list-delete)
  ("o"   . nvp-sessions-list-load)
  ("l"   . nvp-sessions-list-load)
  ("r"   . nvp-sessions-list-rename)
  ("RET" . nvp-sessions-list-load))

(provide 'nvp-session)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-session.el ends here
