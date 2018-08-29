;;; nvp-buffer ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 24 November 2016

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib)
  (require 's))

(declare-function s-suffix? "s")

;;;###autoload
(defun nvp-buffer-kill-other-buffers () 
  (interactive)
    (mapc #'kill-buffer (cdr (buffer-list (current-buffer)))))

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

;; Return list of buffers where `major-mode' matches MODE.
;;;###autoload
(defun nvp-buffer-matching-mode (mode)
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq mode major-mode)
          (push buf buffer-mode-matches))))
    buffer-mode-matches))

;;--- Scratch Buffer -------------------------------------------------

;; Create a new scratch buffer in given mode.
;;;###autoload
(defun nvp-buffer-scratch (mode)
  (interactive (list (if current-prefix-arg
                         (intern
                          (ido-completing-read
                           "Major mode: "
                           (let (r)
                             (mapatoms
                              (lambda (x)
                                (when (s-suffix? "-mode"
                                                 (symbol-name x))
                                  (push x r))))
                             (mapcar 'symbol-name r))))
                       'emacs-lisp-mode)))
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch*"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (call-interactively mode)))

;; Create scratch for current mode.
;;;###autoload
(defun nvp-buffer-scratch-current-mode ()
  (interactive)
  (nvp-buffer-scratch major-mode))

;;--- Buffer Files ---------------------------------------------------

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
