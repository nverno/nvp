;;; nvp-buffer.el --- buffer/file functions -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; buffer/buffer-file commands
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)

;; ------------------------------------------------------------
;;; Utils

(defun nvp-buffer-major-modes ()
  "Return aList of (buffer . `major-mode') for active buffers."
  (cl-loop for buff in (buffer-list)
     collect (cons (buffer-local-value 'major-mode buff) buff)))

;;;###autoload
(defun nvp-buffer-matching-mode (mode)
  "Retun list of buffer with `major-mode' matching MODE."
  (cl-loop for buff in (buffer-list)
     when (eq mode (buffer-local-value 'major-mode (get-buffer buff)))
     collect buff))

;; ------------------------------------------------------------
;;; Kill buffers

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
(defun nvp-buffer-kill-mode-buffers (mode &optional buffs)
  "Kill all buffers of current MODE by default.
With prefix, prompt for MODE buffers to kill."
  (interactive
   (let* ((bufs (nvp-buffer-major-modes))
          (mode (if current-prefix-arg
                    (intern
                     (completing-read
                      "Mode to kill: " (cl-delete-duplicates bufs :key #'car)))
                  major-mode))
          (mbufs (cl-loop for (m . b) in bufs
                          when (eq m mode)
                          collect b)))
     (list mode mbufs)))
  (and (stringp mode) (setq mode (intern mode)))
  (or buffs (setq buffs (nvp-buffer-matching-mode mode)))
  (when (nvp:prompt-with-message "Kill all mode buffers?"
          :read-fn 'y-or-n-p
          :message "%S" (--map (buffer-name it) buffs))
    (message "Killing all %S buffers..." mode)
    (mapc (lambda (buf) (kill-buffer buf)) buffs)))

;; -------------------------------------------------------------------
;;; Buffer files 

;; Delete the current file, and kill the buffer.
;;;###autoload
(defun nvp-buffer-delete-file ()
  (interactive)
  (if-let ((file (buffer-file-name)))
      (when (y-or-n-p (format "Really delete '%s'? " (nvp:path 'bfs)))
        ;; if (vc-backend file)
        ;; (vc-delete-file file)
        (delete-file file delete-by-moving-to-trash)
        (kill-this-buffer))
    (user-error "No file is currently being edited")))

;; Renames both the current buffer and file it's visiting to
;; `NEW-NAME'.
;;;###autoload
(defun nvp-buffer-rename-file (new-name)
  (interactive (list (read-from-minibuffer "New name: " (nvp:path 'bfs))))
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (prog1 (rename-buffer new-name t)
          (message "Buffer '%s' not visiting a file" name))
      (when (file-exists-p filename)
	(rename-file filename new-name t))
      (rename-buffer new-name t)
      (set-visited-file-name new-name t t))))

;; TODO: https://github.com/emacs-helm/helm/blob/master/helm-buffers.el
;; `helm-buffer-toggle-diff'
(defun nvp-buffer-toggle-diff ()
  "Diff buffer with its file."
  ())

;;; Sudo
;; https://github.com/bbatsov/crux/blob/master/crux.el
(defun nvp-sudo-already-root-p ()
  (let ((remote-method (file-remote-p default-directory 'method))
        (remote-user (file-remote-p default-directory 'user)))
    (and remote-method
         (or (member remote-method '("sudo" "su" "ksu" "doas"))
             (and remote-user (string= remote-user "root"))))))

;;;###autoload
(defun nvp-sudo-edit (filename)
  "Edit current file or FILENAME as root.
With prefix ARG, prompt for FILENAME to visit."
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
      (message "Buffer not associated with file")
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
