;;; nvp-buffer.el --- buffer functions -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-02-22 18:27:11>
;; Created: 24 November 2016

;;; Commentary:
;; buffer manipulation commands
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))

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
  (nvp-buffer-kill-other-buffers)
  (kill-buffer))

;;;###autoload
(defun nvp-buffer-kill-other-buffers () 
  "Kill all other buffers."
  (interactive)
  (mapc #'kill-buffer (cdr (buffer-list (current-buffer)))))

;;;###autoload
(defun nvp-buffer-kill-mode-buffers (mode &optional buffs)
  "Kill all MODE buffers."
  (interactive
   (let ((buffs (nvp-buffer-major-modes)))
     (list (ido-completing-read
            "Mode to kill: " (cl-delete-duplicates
                              (mapcar (lambda (s) (symbol-name (car s))) buffs)))
           buffs)))
  (cl-loop for (buff-mode . buff) in (or buffs (nvp-buffer-matching-mode mode))
     with mode = (intern mode)
     when (eq mode buff-mode)
     do (kill-buffer buff)))

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
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (y-or-n-p (format "Really delete '%s'? "
                          (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;; Renames both the current buffer and file it's visiting to
;; `NEW-NAME'.
;;;###autoload
(defun nvp-buffer-rename-file (new-name)
  (interactive
   (list (read-string "New name: " (file-name-nondirectory (buffer-file-name)))))
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
	(message "A buffer named '%s' already exists!" new-name)
      (progn 
	(when (file-exists-p filename)
	  (rename-file filename new-name 1))
	(rename-buffer new-name)
	(set-visited-file-name new-name)))))

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
