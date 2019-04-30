;;; nvp-buffer.el --- buffer functions -*- lexical-binding: t; -*-

;;; Commentary:
;; buffer manipulation commands
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
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
  (save-some-buffers)
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

;; ------------------------------------------------------------
;;; Rearrange buffer windows

;; Transpose the buffers shown in two windows.
;;;###autoload
(defun nvp-buffer-transpose-windows (arg)
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
	    (next-win (window-buffer (funcall selector))))
	(set-window-buffer (selected-window) next-win)
	(set-window-buffer (funcall selector) this-win)
	(select-window (funcall selector)))
      (setq arg (if (cl-plusp arg) (1- arg) (1+ arg))))))

;;;###autoload
(defun nvp-buffer-swap-windows ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win (window-buffer))
             (next-win (window-buffer (next-window)))
             (this-edge (window-edges (selected-window)))
             (next-edge (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-edge)
                                         (car next-edge))
                                     (<= (cadr this-edge)
                                         (cadr next-edge)))))
             (splitter
              (if (= (car this-edge)
                     (car (window-edges (next-window))))
                  #'split-window-horizontally
                #'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win)
          (set-window-buffer (next-window) next-win)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; -------------------------------------------------------------------
;;; Buffer files 

;; Delete the current file, and kill the buffer.
;;;###autoload
(defun nvp-buffer-delete-file ()
  (interactive)
  (if-let ((file (buffer-file-name)))
      (when (y-or-n-p (format "Really delete '%s'? " (nvp-path 'bfs)))
        ;; if (vc-backend file)
        ;; (vc-delete-file file)
        (delete-file file delete-by-moving-to-trash)
        (kill-this-buffer))
    (user-error "No file is currently being edited")))

;; Renames both the current buffer and file it's visiting to
;; `NEW-NAME'.
;;;###autoload
(defun nvp-buffer-rename-file (new-name)
  (interactive (list (read-from-minibuffer "New name: " (nvp-path 'bfs))))
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
