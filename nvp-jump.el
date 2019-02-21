;;; nvp-jump.el --- jumping places -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-02-21 09:39:44>
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

;; Default jump to other window
;; With single prefix, jump same window
;; With double prefix, prompt or something else

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro)
  (nvp-local-vars))
(require 'nvp)
(require 'nvp-read)
(require 'nvp-display)

;; -------------------------------------------------------------------
;;; Modes

;;;###autoload
(defun nvp-jump-to-mode-config (mode action)
  (interactive (list (nvp-read-mode-config "Jump to config: ")
                     (car current-prefix-arg)))
  (nvp-display-location (nvp-mode-config-path mode) :file action))

;; Jump to test with extension `STR'.  If it doesn't exist make a new
;; file, and if there are multiple matches offer ido choice.
;;;###autoload
(defun nvp-jump-to-mode-test (test action)
  "Jump to TEST file for mode, creating one if necessary."
  (interactive (list (nvp-read--mode-test) (car current-prefix-arg)))
  (nvp-display-location test :file action))

;;;###autoload
(defun nvp-jump-to-mode-hook (mode action)
  "Jump to location defining MODEs hook."
  (interactive
   (list (if (eq (car current-prefix-arg) 16) (nvp-read-mode) major-mode)
         (car current-prefix-arg)))
  (and (stringp mode) (setq mode (intern mode)))
  (let* ((str-mode-hook (format "%s-hook" mode))
         (hook-fn-name (format "nvp-%s-hook" (substring (symbol-name mode) 0 -5)))
         (hook-fn (intern-soft hook-fn-name)))
    (nvp-display-location (or hook-fn nvp-default-hooks-file) :find-func action)
    (goto-char (point-min))
    (search-forward str-mode-hook nil t)))

;; -------------------------------------------------------------------
;;; Install / build files

;;;###autoload
(defun nvp-jump-to-mode-install (file action)
  "Jump to external installation files for MODE.
With double prefix, prompt for mode."
  (interactive (list (nvp-read--mode-install
                      (if (eq (car current-prefix-arg) 16)
                          (substring (nvp-read-mode) 0 -5)))
                     (car current-prefix-arg)))
  (nvp-display-location file :file action))

;;;###autoload
(defun nvp-jump-to-build-file (file action)
  "Jump with ACTION to an init FILE in the build directory."
  (interactive
   (list (nvp-read-relative-recursively
          nvp-build-init-dir ".el$" "Jump to init file: ")
         (car current-prefix-arg)))
  (nvp-display-location file :file action))

;; -------------------------------------------------------------------
;;; Org / Info

;;;###autoload
(defun nvp-jump-to-org (org-file action)
  "Jump to org file. 
If `nvp-local-notes-file' is bound use that unless there is a prefix of 16. 
Otherwise prompt, with default `nvp-default-org-file'."
  (interactive
   (list (nvp-read--org-file nil nil (eq 16 (car current-prefix-arg)))
         (car current-prefix-arg)))
  (with-current-buffer (nvp-display-location org-file :file action)
    (goto-char (point-min))
    (ignore-errors (search-forward "* Notes"))))

;;;###autoload
(defun nvp-jump-to-info (file action)
  "Jump to info file (in org mode). 
With prefix jump this window, otherwise `find-file-other-window'."
  (interactive (list (nvp-read--info-files) (car current-prefix-arg)))
  (nvp-display-location file :file action))

;; -------------------------------------------------------------------
;;; Scratch

;;;###autoload
(defun nvp-jump-to-scratch (mode action)
  "Jump to scratch buffer in MODE (default current `major-mode'). 
With prefix, pop other window, with double prefix, prompt for MODE."
  (interactive
   (list (if (eq (car current-prefix-arg) 16) (intern (nvp-read-mode))
           major-mode)
         (car current-prefix-arg)))
  (let ((buff (get-buffer-create "*scratch*")))
    (with-current-buffer buff
      (if (eq mode 'emacs-lisp-mode)
          (unless (eq major-mode 'lisp-interaction-mode)
            (lisp-interaction-mode))
        (let ((inhibit-read-only t))
          (kill-all-local-variables)
          (erase-buffer)
          (funcall mode)))
      (local-set-key (kbd "C-c C-c") #'kill-this-buffer)
      (local-set-key
       (kbd "C-c C-s") (lambda () (interactive) (funcall (intern (nvp-read-mode)))))
      (message "%s" (substitute-command-keys
                     "Press \\[kill-this-buffer] to kill this buffer \
or C-c C-s to switch major modes. "))
      (nvp-display-location buff :buffer action))))

;; -------------------------------------------------------------------
;;; Books / PDFs

;;;###autoload
(defun nvp-jump-to-book (dir &optional action)
  "Jump to book, either opening in emacs (eg. pdfs) or external for epubs.
With double prefix, prompt for directory (default `nvp-books-local-directory'
or `nvp/books'. 
With triple prefix, offer recursive results."
  (interactive
   (let* ((arg (or (car current-prefix-arg) 0))
          (case-fold-search t)
          (root (cond
                  ((> arg 4)
                   (expand-file-name
                    (read-directory-name "Book Directory: " nvp/books)))
                  ((bound-and-true-p nvp-books-local-directory)
                   nvp-books-local-directory)
                  (t (expand-file-name "programming" nvp/books)))))
     (list root arg)))
  (let* ((files (mapcar (lambda (f) (file-relative-name f dir))
                        (if (eq action 16) ;recurse
                            (directory-files-recursively dir "^[^.].*[^/]$")
                          (directory-files dir t "^[^.]"))))
         (book (nvp-completing-read "Book: " files nil 'match))
         (fullname (expand-file-name book dir)))
    (cond
      ;; epubs
      ((string-match-p "\\.epub$" book)
       (nvp-with-gnu/w32
           (if (executable-find "calibre")
               (call-process "calibre" nil 0 nil fullname)
             (call-process "firefox" nil 0 nil fullname))
         (if (executable-find "sumatrapdf")
             (call-process "sumatrapdf" nil 0 nil fullname)
           (call-process (nvp-program "firefox") nil 0 nil fullname))))
      ;; probably PDF, open in emacs
      ((file-name-extension book)
       (nvp-display-location fullname :file action))
      ;; otherwise recurse in subdirs
      (t (nvp-jump-to-book fullname action)))))

;; -------------------------------------------------------------------
;;; Other

;;;###autoload
(defun nvp-jump-to-register (action)
  (interactive (list (car current-prefix-arg)))
  (nvp-display-with-action (or (not (eq action 4)) 4)
    (setq prefix-arg current-prefix-arg)
    (call-interactively #'jump-to-register)))

;;;###autoload
(defun nvp-jump-to-dir (dir action)
  (interactive
   (list (if (eq (car current-prefix-arg) 16) nvp/project nvp/class)
         (car current-prefix-arg)))
  (nvp-display-location dir :ido action #'ido-find-file-in-dir))

;;;###autoload
(defun nvp-jump-to-template (action)
  (interactive (list (car current-prefix-arg)))
  (nvp-display-location nvp/template :ido action #'ido-find-file-in-dir))

(provide 'nvp-jump)
;;; nvp-jump.el ends here
