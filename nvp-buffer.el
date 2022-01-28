;;; nvp-buffer.el --- buffer/file functions -*- lexical-binding: t; -*-

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
   (let ((buffs (nvp-buffer-major-modes)))
     (if (not current-prefix-arg)
         (list major-mode buffs)
       (list (intern
              (completing-read
               "Mode to kill: " (cl-delete-duplicates buffs :key #'car)))
             buffs))))
  (message "Killing all %S buffers..." mode)
  (and (stringp mode) (setq mode (intern mode)))
  (or buffs (setq buffs (nvp-buffer-matching-mode mode)))
  (pcase-dolist (`(,bmode . ,buff) (or buffs (nvp-buffer-matching-mode mode)))
    (and (eq mode bmode) (kill-buffer buff))))

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

(eval-when-compile
 (defmacro nvp-sudo:wrap (func &optional filename)
   "Wrap file finding FUNC to open FILENAME as root."
   `(let ((remote-method (file-remote-p default-directory 'method))
          (remote-host (file-remote-p default-directory 'host))
          (remote-localname (file-remote-p default-directory 'localname)))
      (,func (format "/%s:root@%s:%s"
                     (or remote-method "sudo")
                     (or remote-host "localhost")
                     (or remote-localname
                         ,(or filename '(read-file-name "Find file (root): "))))))))

(defun nvp-sudo-already-root-p ()
  (let ((remote-method (file-remote-p default-directory 'method))
        (remote-user (file-remote-p default-directory 'user)))
    (and remote-method
         (or (member remote-method '("sudo" "su" "ksu" "doas"))
             (string= remote-user "root")))))

;; https://github.com/bbatsov/crux/blob/master/crux.el
;;;###autoload
(defun nvp-sudo-edit (&optional arg)
  "Edit current file as root.
With prefix ARG, prompt for file to visit."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (nvp-sudo:wrap find-file)
    (if (nvp-sudo-already-root-p)
        (message "Already editing file as root.")
      (let ((place (point)))
        (nvp-sudo:wrap find-alternate-file (buffer-file-name))
        (goto-char place)))))

;; -------------------------------------------------------------------
;;; Encoding 

;; convert buffer coding to utf-8 and removing all trailing '\r'
;;;###autoload
(defun nvp-buffer-convert-to-utf-8 ()
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
;;; nvp-buffer.el ends here
