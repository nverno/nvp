;;; nvp-jump.el --- jumping places -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-02-13 10:52:08>
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
                                   . display-buffer-same-window))
      (find-file location)))
   ((pred functionp) (funcall action location))
   (_                                   ;different window
    (if buffer-p
        (display-buffer location '(display-buffer-pop-up-window
                                   . inhibit-same-window))
      (find-file-other-window location)))))

;; -------------------------------------------------------------------
;;; Modes

;;;###autoload
(defun nvp-jump-to-mode-config (mode action)
  (interactive
   (list
    (let ((mmode (and major-mode (symbol-name major-mode))))
      (nvp-completing-read
       "Mode: "
       (mapcar
        #'(lambda (x)
            ;; ignore preceding 'nvp-' and ending '-config.el'
            (replace-regexp-in-string "\\(nvp-\\|\\(?:-config\\)?\\.el\\)" "" x))
        (directory-files nvp/config nil "^[^\\.].*\\.el$"))
       nil nil (substring mmode 0 -5)))
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
(defun nvp-jump-to-info (&optional arg)
  "Jump to info file (in org mode).
With prefix jump this window, otherwise `find-file-other-window'."
  (interactive "P")
  (let* ((org (nvp-completing-read
               "Info file: "
               (directory-files (expand-file-name "org" nvp/info) nil "\.org")))
         (file (expand-file-name org (expand-file-name "org" nvp/info))))
    (if arg (find-file file)
      (find-file-other-window file))))

;; -------------------------------------------------------------------
;;; Scratch

;;;###autoload
(defun nvp-jump-to-scratch (mode &optional action)
  "Jump to scratch buffer in MODE (default current `major-mode').
With prefix, pop other window, with double prefix, prompt for MODE."
  (interactive
   (list (if (eq (car current-prefix-arg) 16) (intern (nvp-read-mode))
           major-mode)
         (eq (car current-prefix-arg) 4)))
  (pop-to-buffer (get-buffer-create "*scratch*") action)
  (if (eq mode 'emacs-lisp-mode)
      (unless (eq major-mode 'lisp-interaction-mode)
        (lisp-interaction-mode))
    (let ((inhibit-read-only t))
      (kill-all-local-variables)
      (erase-buffer)
      (funcall mode)))
  (local-set-key (kbd "C-c C-c") #'kill-this-buffer))

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
