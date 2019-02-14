;;; nvp-jump.el --- jumping places -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-02-13 18:30:19>
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

;; With single prefix, jump same window
;; With double prefix, prompt

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro)
  (nvp-local-vars))
(require 'nvp)
(declare-function find-function-other-window "find-func")
(declare-function nvp-read-mode "nvp-read")
(autoload 'string-remove-prefix "subr-x")

;; actions to take jumping to buffers/files
(defun nvp-jump--location (location buffer-p action)
  (pcase action
   (`4                                  ;same window
    (if buffer-p
        (display-buffer location '(display-buffer-same-window
                                   ((inhibit-switch-frame . nil))
                                   ((inhibit-same-window  . nil))))
      (find-file location)))
   ((pred functionp) (funcall action location))
   (_                                   ;different window
    (if buffer-p
        (display-buffer location '(display-buffer-pop-up-window
                                   ((inhibit-same-window  . t))))
      (find-file-other-window location)))))

;; -------------------------------------------------------------------
;;; Modes

(defvar nvp-mode-config-history ())

;;;###autoload
(defun nvp-jump-to-mode-config (mode action)
  (interactive
   (list
    (let ((mmode (and major-mode (symbol-name major-mode))))
      (ido-completing-read
       (if mmode (format "Mode (default %s): " mmode) "Mode: ")
       (mapcar
        #'(lambda (x)
            ;; ignore preceding 'nvp-' and ending '-config.el'
            (replace-regexp-in-string "\\(nvp-\\|\\(?:-config\\)?\\.el\\)" "" x))
        (directory-files nvp/config nil "^[^\\.].*\\.el$"))
       nil nil nil 'nvp-mode-config-history (substring mmode 0 -5)))
    (car current-prefix-arg)))
  (nvp-jump--location (nvp-mode-config-path mode) nil action))

;; Jump to test with extension `STR'.  If it doesn't exist make a new
;; file, and if there are multiple matches offer ido choice.
;;;###autoload
(defun nvp-jump-to-mode-test (test action)
  "Jump to TEST file for mode, creating one if necessary."
  (interactive
   (let* ((files (let ((default-directory nvp/test)
                       (completion-ignored-extensions
                        (cons "target" completion-ignored-extensions)))
                   (mapcar (lambda (f) (file-relative-name f nvp/test))
                           (directory-files-recursively nvp/test "^[^.][^.]"))))
          (test (nvp-completing-read "Test: " files)))
     (list test (car current-prefix-arg))))
  (nvp-jump--location (expand-file-name test nvp/test) nil action))

;;;###autoload
(defun nvp-jump-to-mode-hook (mode action)
  "Jump to location defining MODEs hook."
  (interactive
   (list (if (eq (car current-prefix-arg) 16) (nvp-read-mode) major-mode)
         (car current-prefix-arg)))
  (and (stringp mode) (setq mode (intern mode)))
  (let* ((str-mode-hook (format "%s-hook" mode))
         (hook-fn-name (format "nvp-%s-hook" (substring (symbol-name mode) 0 -5)))
         (hook-fn (intern-soft hook-fn-name))
         (action (if (eq 4 action) #'find-function #'find-function-other-window)))
    (if hook-fn
        (nvp-jump--location hook-fn nil action)
      (nvp-jump--location
       (expand-file-name "nvp-mode-hooks.el" nvp/lisp) nil action)
      (goto-char (point-min))
      (search-forward str-mode-hook nil t))))

;; -------------------------------------------------------------------
;;; Org

;;;###autoload
(defun nvp-jump-to-org (org-file action &optional local-file)
  (interactive
   (let* ((choose (eq 16 (car current-prefix-arg)))
          (local-file (bound-and-true-p nvp-local-notes-file))
          (org (if choose
                   (nvp-completing-read
                    "Org file: "
                    (delq nil (cons local-file
                                    (directory-files nvp/org nil "^[^.]"))))
                 "gtd.org"))
          (file (or (and (not choose) local-file)
                    (expand-file-name org nvp/org))))
     (list file (car current-prefix-arg) local-file)))
  (with-current-buffer (nvp-jump--location org-file nil action)
    (unless local-file
      (goto-char (point-min))
      (search-forward "* Notes" nil 'move))))

;;;###autoload
(defun nvp-jump-to-info (file action)
  "Jump to info file (in org mode)                               . 
With prefix jump this window, otherwise `find-file-other-window' . "
  (interactive
   (let* ((file (nvp-completing-read
                 "Info file: "
                 (directory-files (expand-file-name "org" nvp/info) nil "\.org")))
          (file (expand-file-name file (expand-file-name "org" nvp/info))))
     (list file (car current-prefix-arg))))
  (nvp-jump--location file nil action))

;; -------------------------------------------------------------------
;;; Scratch

;;;###autoload
(defun nvp-jump-to-scratch (mode action)
  "Jump to scratch buffer in MODE (default current `major-mode')   . 
With prefix, pop other window, with double prefix, prompt for MODE . "
  (interactive
   (list (if (eq (car current-prefix-arg) 16) (intern (nvp-read-mode))
           major-mode)
         (car current-prefix-arg)))
  (let ((buff (get-buffer-create "*scratch*")))
   (with-current-buffer (window-buffer (nvp-jump--location buff 'buffer action))
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
     (pop-to-buffer (current-buffer)))))

;; -------------------------------------------------------------------
;;; Books / PDFs

;; FIXME: when opening in calibre, import is annoying - alternative to calibre?
;;;###autoload
(defun nvp-jump-to-book (dir &optional action)
  "Jump to book, either opening in emacs (eg. pdfs) or external for epubs.
With double prefix, prompt for directory (default `nvp-books-local-directory'
or `nvp/books'.
With triple prefix, offer recursive results."
  (interactive
   (let* ((arg (or (car current-prefix-arg) 0))
          (case-fold-search t)
          ;; (mode (substring (substring-no-properties (symbol-name major-mode)) 0 -5))
          (locals (bound-and-true-p nvp-books-local-directory))
          (root (cond
                 (root root)
                 ((> arg 4)
                  (expand-file-name
                   (read-directory-name "Book Directory: " nvp/books)))
                 (locals locals)
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
      (nvp-jump--location fullname nil action))
     ;; otherwise recurse in subdirs
     (t (nvp-jump-to-book fullname action)))))

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
