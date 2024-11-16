;;; nvp-buffer.el --- buffer functions -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls)

(defun nvp-buffer-major-modes ()
  "Return alist of \\='(buffer . major-mode) for active buffers."
  (cl-loop for buff in (buffer-list)
           collect (cons (buffer-local-value 'major-mode buff) buff)))

;;;###autoload
(defun nvp-buffer-matching-mode (mode &optional include-hidden)
  "Retun list of buffer with `major-mode' matching MODE."
  (cl-loop for buf in (buffer-list)
           when (and (or include-hidden (nvp:buffer-visible-p buf))
                     (eq mode (buffer-local-value 'major-mode buf)))
           collect buf))

;;;###autoload
(defun nvp-buffer-kill-all-buffers ()
  "Kill all buffers including this one, calling `save-some-buffers' first."
  (interactive)
  (save-some-buffers 'no-ask)
  (let ((kill-buffer-query-functions ()))
    (nvp-buffer-kill-other-buffers)
    (kill-buffer (current-buffer))))

;;;###autoload
(defun nvp-buffer-kill-other-buffers ()
  "Kill all buffers other than the current one in this frame."
  (interactive)
  (mapc #'kill-buffer (cdr (buffer-list (current-buffer)))))

;;;###autoload
(defun nvp-buffer-kill-mode-buffers (mode &optional include-hidden)
  "Kill all buffers whose `major-mode' is MODE, defaulting to current.
If INCLUDE-HIDDEN is non-nil (>= 2 prefix), also kill hidden buffers
matching MODE. Prompt for MODE with prefix."
  (interactive (list (if current-prefix-arg
                         (nvp-read-mode "Kill buffers of mode: ")
                       major-mode)
                     (> (prefix-numeric-value current-prefix-arg) 4)))
  (if-let* ((buffs (nvp-buffer-matching-mode (if (stringp mode)
                                                 (intern mode)
                                               mode)
                                             include-hidden))
            (names (mapconcat #'buffer-name buffs ", ")))
      (when (nvp:prompt-with-message
                (format "Kill mode buffers (%d)? " (length buffs))
              :read-fn 'y-or-n-p
              :message (let ((w (- (window-width) 10)))
                         (if (length> names w)
                             (concat (substring names 0 w) "...")
                           names)))
        (mapc (lambda (buf) (kill-buffer buf)) buffs)
        (message "Killed %S buffers: %s" mode names))
    (user-error "No buffers of %s" mode)))

;;;###autoload
(defun nvp-buffer-delete-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (if-let* ((file (buffer-file-name)))
      (when (y-or-n-p (format "Really delete %s? " (nvp:path 'bfs)))
        (delete-file file delete-by-moving-to-trash)
        (kill-current-buffer))
    (user-error "No file is currently being edited")))

;;;###autoload
(defun nvp-buffer-rename-file (new-name)
  "Renames both the current buffer and file it's visiting to `NEW-NAME'."
  (interactive (list (read-from-minibuffer "New name: " (nvp:path 'bfs))))
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (prog1 (rename-buffer new-name t)
          (message "No file for buffer %s" name))
      (when (file-exists-p filename)
	(rename-file filename new-name t))
      (rename-buffer new-name t)
      (set-visited-file-name new-name t t))))

;; XXX(09/06/24): something like `helm-buffer-toggle-diff'
;; https://github.com/emacs-helm/helm/blob/master/helm-buffers.el
;; (defun nvp-buffer-toggle-diff () "Diff buffer with its file.")

(defun nvp-sudo-already-root-p ()
  (let ((remote-method (file-remote-p default-directory 'method))
        (remote-user (file-remote-p default-directory 'user)))
    (and remote-method
         (or (member remote-method '("sudo" "su" "ksu" "doas"))
             (and remote-user (string= remote-user "root"))))))

;; From https://github.com/bbatsov/crux/blob/master/crux.el
;;;###autoload
(defun nvp-sudo-edit (filename)
  "Edit current file or FILENAME as root.
Prompt for FILENAME to with prefix."
  (interactive
   (list (if (or current-prefix-arg (null buffer-file-name))
             (read-file-name "Find file (root): ")
           buffer-file-name)))
  (let* ((remote-method (or (file-remote-p default-directory 'method) "sudo"))
         (remote-host (or (file-remote-p default-directory 'host) "localhost"))
         (func (if (or current-prefix-arg
                       (not (equal buffer-file-name filename)))
                   #'find-file
                 #'find-alternate-file))
         (pos (and (eq func 'find-alternate-file) (point))))
    (when (and (eq func 'find-alternate-file)
               (nvp-sudo-already-root-p))
      (user-error "already sudoing %s" filename))
    (funcall func (format "/%s:root@%s:%s"
                          (or remote-method "sudo")
                          (or remote-host "localhost")
                          (or (file-remote-p filename 'localname) filename)))
    (and pos (goto-char pos))))

;;;###autoload
(defun nvp-buffer-convert-to-utf-8 ()
  "Convert buffer coding to utf-8 and removing all trailing \"\\r\"."
  (interactive)
  (if (not (buffer-file-name))
      (user-error "Buffer not associated with file")
    (revert-buffer-with-coding-system 'utf-8-unix)
    (save-excursion
      (goto-char (point-min))
      (save-match-data
        (while (search-forward "" nil 'move)
          (replace-match ""))))))

(provide 'nvp-buffer)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-buffer.el ends here
