;;; nvp-bookmark.el --- jump b/w boomark files -*- lexical-binding: t; -*-

;; Last modified: <2019-03-31 00:45:14>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 24 November 2016

;;; Commentary:

;; - bookmark-to-bookmark jump handler
;; - assorted bookmark functions

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'nvp)
(require 'bookmark)

;; -------------------------------------------------------------------
;;; Jumping b/w bookmark files

(defgroup nvp-bmk nil
  "Manage jumping between/bookmarking multiple bookmark files."
  :group 'bookmark
  :prefix "nvp-bmk-")

(defface nvp-bmk-bookmark-highlight
  '((((background dark)) (:background "light blue" :foreground "black"))
    (t (:background "light blue")))
  "Face to highlight bookmark entries (.bmk)."
  :group 'nvp-bmk)

;; store bookmark files
(nvp-setup-cache nvp-bmk-default-directory "bookmarks")
(defvar nvp-bmk-regexp "^.*\\.bmk$" "Regexp to match bookmark entries.")
(defvar nvp-bmk-stack ())

;; Create bookmark record for bookmark-menu-list from current default
(defun nvp-bmk-record-function ()
  `((filename . ,bookmark-default-file)
    (handler  . nvp-bmk-handler)))

(defun nvp-bmk-handler (bmk-record)
  (push bookmark-default-file nvp-bmk-stack)
  (when (> bookmark-alist-modification-count 0)
    (bookmark-save))
  (setq bookmark-default-file (bookmark-get-filename bmk-record))
  (setq bookmark-alist nil)
  (let (bookmarks-already-loaded)
    (bookmark-maybe-load-default-file))
  (bookmark-bmenu-list))

(defun nvp-bmk-make-record (filename &optional name)
  (cons (or name nil)
   `((filename . ,filename)
     (handler  . nvp-bmk-handler))))

(defun nvp-bmk-back ()
  "Go back to last bookmark file, saving current if modified."
  (interactive)
  (when (> (length nvp-bmk-stack) 0)
    (when (> bookmark-alist-modification-count 0)
      (bookmark-save))
    (setq bookmark-default-file (pop nvp-bmk-stack))
    (setq bookmark-alist nil)
    (let (bookmarks-already-loaded)
      (bookmark-maybe-load-default-file))
    (bookmark-bmenu-list)))

(defun nvp-bmk-new (filename &optional current link)
  "Create new bookmark file, prompting for FILENAME. If `current-prefix-arg' is 
4 or CURRENT is non-nil, set new bookmark file as current default bookmark file.  
If `current-prefix-arg' is 16 or LINK is non-nil, create link to new 
bookmark file from current bookmark menu list."
  (interactive
   (list
    (let ((default-directory (or nvp-bmk-default-directory
                                 default-directory)))
      (read-file-name "Bookmark File: "))))
  (when (not (file-exists-p filename))
    (message "Creating new bookmark file at %s" filename)
    (with-temp-buffer
      (let (bookmark-alist)
        (bookmark-save nil filename))))
  (when (or link (equal current-prefix-arg '(16)))
    (let* ((name (read-from-minibuffer "Bookmark Link Name: "))
           (record (nvp-bmk-make-record filename)))
      (bookmark-store name (cdr record) t)))
  (when (or current (equal current-prefix-arg '(4)))
    (nvp-bmk-handler
     (nvp-bmk-make-record filename))))

;; Font-lock
(defvar-local nvp-bmk-highlighted nil)

(defun nvp-bmk-toggle-highlight ()
  "Toggle highlighting of bookmark entries in *Bookmark List* buffer."
  (interactive)
  (if (not nvp-bmk-highlighted)
      (progn
        (nvp-bmk-add-highlight nvp-bmk-regexp 'nvp-bmk-bookmark-highlight)
        (setq nvp-bmk-highlighted t))
    (nvp-bmk-remove-highlight)
    (setq nvp-bmk-highlighted nil)))

(defun nvp-bmk-add-highlight (regexp face)
  "Highlight bookmark entries in *Bookmark List* buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put overlay 'face face))
      (goto-char (match-end 0)))))

(defun nvp-bmk-remove-highlight ()
  "Remove highlighting of bookmark entries."
  (remove-overlays))


;;;###autoload
(define-minor-mode nvp-bmk-to-bmk
  "Toggle `nvp-bmk-to-bmk' mode.
Interactively with no arguments, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix 
argument disables it.  From lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When `nvp-bmk-to-bmk' mode is enabled, bookmark menus can be both bookmarked
and jumped between."
  :keymap '(((kbd "b") . nvp-bmk-back)
            ((kbd "n") . nvp-bmk-new)
            ((kbd "f") . nvp-bmk-toggle-highlight))
  :lighter " B2B"
  (setq-local bookmark-make-record-function 'nvp-bmk-record-function))

;;;###autoload
(add-hook 'bookmark-bmenu-mode-hook #'nvp-bmk-to-bmk)

;; -------------------------------------------------------------------
;;; Commands

;;;###autoload
(defun nvp-bookmark-local (file)
  (interactive
   (list (or (and current-prefix-arg
                  (read-file-name "Location of bookmark file: "))
             (and (bound-and-true-p nvp-local-bookmark-file)
                  (expand-file-name
                   nvp-local-bookmark-file
                   (locate-dominating-file default-directory dir-locals-file)))
             bookmark-default-file)))
  (message "Using bookmark file: %s" file)
  (when (not (string= bookmark-default-file file))
    (nvp-bmk-handler (nvp-bmk-make-record file)))
  (call-interactively 'bookmark-bmenu-list))

(provide 'nvp-bookmark)
;;; nvp-bookmark.el ends here
