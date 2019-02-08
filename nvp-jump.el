;;; nvp-jump.el --- jumping places -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-02-08 04:28:19>
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
  (require 'cl-lib)
  (require 'nvp-macro)
  (nvp-local-vars))
(require 'nvp)
(autoload 'find-function-other-window "find-func")
(autoload 'string-remove-prefix "subr-x")

;; -------------------------------------------------------------------
;;; Modes

;;;###autoload
(defun nvp-jump-to-mode-config (mode)
  (interactive
   (list 
    (nvp-completing-read
     "Mode: "
     (mapcar
      #'(lambda (x)
          (replace-regexp-in-string "\\(nvp-\\|\\(?:-config\\)?\\.el\\)" "" x))
      (directory-files nvp/mode nil "^[^\\.].*\\.el$"))
     nil nil
     (and current-prefix-arg major-mode
          (substring (symbol-name major-mode) 0 -5)))))
  (let ((src (format "nvp-%s-config.el" mode)))
    (find-file-other-window (expand-file-name src nvp/mode))))


;; Jump to test with extension `STR'.  If it doesn't exist make a new
;; file, and if there are multiple matches offer ido choice.
;;;###autoload
(defun nvp-jump-to-mode-test (str)
  (interactive "sExtension: ")
  (let* ((test-files (directory-files nvp/test
                                      t (concat "\\." str "$")))
         (alt-files (or (and (or (not test-files))
                             (directory-files-recursively
                              nvp/test (concat "\\." str "$") nil))
                        (concat
                         nvp/test (format "test_%s.%s" str str))))
         (files (or test-files alt-files))
         (file (if (and (listp files) (> (length files) 1))
                   (ido-completing-read "Options: " files)
                 (or (and (eq 1 (length files))
                          (car files))
                     files))))
    (find-file-other-window file)))

;; Jump to where the hook for this mode is defined.
;;;###autoload
(defun nvp-jump-to-mode-hook ()
  (interactive)
  (let* ((str-mode-hook (format "%s-hook" major-mode))
         (hook-fn-name
          (format "nvp-%s-hook" (substring (symbol-name major-mode) 0 -5)))
         (hook-fn (intern-soft hook-fn-name)))
    (if hook-fn (find-function-other-window hook-fn)
      (find-file (expand-file-name "nvp-mode-hooks.el" nvp/lisp))
      (goto-char (point-min))
      (search-forward str-mode-hook nil t))))

;; -------------------------------------------------------------------
;;; Org

(defvar-local notes-file nil)

;;;###autoload
(defun nvp-jump-to-org (arg)
  (interactive "P")
  (let* ((org (or (and arg
                       (ido-completing-read
                        "Org file: "
                        (directory-files nvp/org nil "^[^.]")))
                  "gtd.org"))
         (file (or (and (not arg) (bound-and-true-p notes-file))
                   (expand-file-name org nvp/org))))
    (when (file-exists-p file)
      (find-file-other-window file)
      (unless (bound-and-true-p notes-file)
        (goto-char (point-min))
        (search-forward "* Notes" nil 'move)))))

;;;###autoload
(defun nvp-jump-to-info ()
  (interactive)
  (let* ((org (ido-completing-read
               "Info file: "
               (directory-files (expand-file-name "org" nvp/info) nil "\.org")))
         (file (expand-file-name org (expand-file-name "org" nvp/info))))
    (find-file-other-window file)))

;; -------------------------------------------------------------------
;;; Scratch
(autoload 'nvp-read-obarray "nvp-read")

;;;###autoload
(defun nvp-jump-to-scratch (mode)
  (interactive
   (list (if current-prefix-arg
             (nvp-read-obarray "Major mode: " "-mode\\'")
           (symbol-name major-mode))))
  (if (string= mode "emacs-lisp-mode")
      (switch-to-buffer (get-buffer-create "*scratch*"))
    (switch-to-buffer (get-buffer-create (concat "*scratch-" mode)) "*")
    (call-interactively (intern mode))))

;; -------------------------------------------------------------------
;;; Books / PDFs

;; Jump to book in directory:
;; 1. `nvp/books'/DIRNAME if non-nil.
;; 2. Prompt for directory with prefix arg (appends `nvp/books')
;; 3. Check for variable 'books-directory'
;; 4. Default to 'nvp/books'.
;; Opens epubs in external program - calibre / sumatrapdf
;; FIXME: when opening in calibre, import is annoying - alternative
;;        to calibre?
;;;###autoload
(defun nvp-jump-to-book (dirname &optional recurse)
  (interactive
   (list
    (or (bound-and-true-p dirname)
        ;; select root directory
        (and (member current-prefix-arg '((16) (64)))
             (expand-file-name
              (read-directory-name
               "Book Directory (default book root): "
               nvp/books)))
        ;; local variable
        (and (bound-and-true-p books-directory)
             (expand-file-name books-directory))
        ;; default
        nvp/books)
    (and (member current-prefix-arg '((4) (64))))))
  (let* ((files (if recurse
                    ;; exclude directories
                    (directory-files-recursively
                     dirname "^[^.].*[^/]$")
                  (directory-files dirname t "^[^.]")))
         (shortnames
          ;; if recursing, use name after DIRNAME for completing-read
          ;; otherwise, just use short name
          (mapcar (if recurse
                      #'(lambda (s)
                          (string-remove-prefix dirname s))
                    'file-name-nondirectory)
                  files))
         (mode (substring-no-properties (symbol-name major-mode) 0 -5))
         (case-fold-search t))
    (when (and mode (member mode shortnames))
      (setq dirname (expand-file-name mode dirname))
      (setq shortnames (directory-files dirname nil "^[^.]")))
    (when shortnames
      (let* ((file (ido-completing-read "Book: " shortnames))
             (fullname (expand-file-name file dirname)))
        (cond
         ;; epubs
         ((string-match-p "\\.epub$" file)
          (nvp-with-gnu/w32
           (if (executable-find "calibre")
               (call-process "calibre" nil 0 nil fullname)
             (call-process "firefox" nil 0 nil fullname))
           (if (executable-find "sumatrapdf")
               (call-process "sumatrapdf" nil 0 nil fullname)
             (call-process (nvp-program "firefox") nil 0 nil fullname))))
         ;; open if non-directory
         ((file-name-extension file)
          (find-file fullname))
         ;; otherwise assume its a directory
         (t (nvp-jump-to-book fullname)))))))

;; -------------------------------------------------------------------
;;; Other

;; jump to register in other window
;;;###autoload
(defun nvp-jump-to-register (&rest _args)
  (interactive "P")
  (cl-letf (((symbol-function 'switch-to-buffer) 'switch-to-buffer-other-window)
            ((symbol-function 'pop-to-buffer-same-window) 'pop-to-buffer))
    (setq prefix-arg current-prefix-arg)
    (call-interactively 'jump-to-register)))

;;;###autoload
(defun nvp-jump-to-dir (arg)
  (interactive "P")
  (ido-find-file-in-dir (if arg nvp/project nvp/class)))

;;;###autoload
(defun nvp-jump-to-template ()
  (interactive)
  (ido-find-file-in-dir nvp/template))

(provide 'nvp-jump)
;;; nvp-jump.el ends here
