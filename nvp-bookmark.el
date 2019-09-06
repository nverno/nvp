;;; nvp-bookmark.el --- jump b/w boomark files -*- lexical-binding: t; -*-

;;; Commentary:
;; - bookmark-to-bookmark jump handler
;; - assorted bookmark functions
;; #<marker at 210151 in info.el.gz>
;; FIXME:
;; - wrong formatting with xrefs
;; TODO:
;; - create new bookmark files
;; - interface to list bookmark files
;; - show bookmarks for a given file
;; - update with new bookmark.el modifications
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'nvp-cache)
(require 'bookmark)
(autoload 'f-same-p "f")

(defvar nvp-bookmark-directory (expand-file-name "bookmarks" nvp/cache))

(defun nvp-bookmark-locate-file (file &optional ignore-local ok-not-exists)
  (or file (setq file bookmark-default-file))
  (if (file-name-absolute-p file) file
    (if (or ignore-local (not (bound-and-true-p nvp-local-bookmark-file)))
        (expand-file-name file nvp-bookmark-directory)
      (let ((f (expand-file-name
                nvp-local-bookmark-file
                (locate-dominating-file default-directory dir-locals-file))))
        (if (or ok-not-exists (file-exists-p f)) f
          (expand-file-name nvp-local-bookmark-file nvp-bookmark-directory))))))

;;;###autoload
(defun nvp-bookmark-local (file)
  "Load bookmark FILE.
With prefix prompt for filename. Otherwise, try `nvp-local-bookmark-file'
and fallback to `bookmark-default-file'."
  (interactive
   (list (or (and current-prefix-arg (read-file-name "Bookmark file: "))
             (nvp-bookmark-locate-file nvp-local-bookmark-file))))
  (nvp-bmk-update-history file)
  (unless (f-same-p bookmark-default-file file)
    (nvp-bmk-handler (nvp-bmk-make-record file)))
  (call-interactively 'bookmark-bmenu-list))

;;;###autoload
(defun nvp-bookmark-list (&optional reload)
  "With arg reload defaults."
  (interactive "P")
  (when reload
    (setq bookmark-bookmarks-timestamp nil
          bookmark-default-file
          (expand-file-name
           "bookmarks-linux.bmk" (nvp-path 'dn bookmark-default-file))
          bookmark-alist nil)
    (message "Reloaded: %s" bookmark-default-file))
  (let ((bookmark-default-file (nvp-bookmark-locate-file nvp-local-bookmark-file)))
    (call-interactively #'bookmark-bmenu-list)))


;; -------------------------------------------------------------------
;;; BMK handling

;; (define-advice bookmark-load-hook)

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
(nvp-setup-cache nvp-bmk-directory "bookmarks")
(defvar nvp-bmk-cache
  (nvp-cache-create 'ring
    :filename ".bmk_history"
    :insert-filter #'abbreviate-file-name
    :save-predicate #'file-exists-p)
  "Cache bookmark lists.")

(defvar nvp-bmk-regexp "^.*\\.bmk$" "Regexp to match bookmark entries.")

(defsubst nvp-bmk-update-history (&optional filename silent)
  (nvp-cache-insert (or filename bookmark-default-file) nvp-bmk-cache)
  (or silent
      (message "Current bookmark file: %s"
               (abbreviate-file-name (or filename bookmark-default-file)))))

;; Create bookmark record for bookmark-menu-list from current default
(defun nvp-bmk-record-function ()
  `((filename . ,(bookmark-buffer-file-name))
    (handler  . nvp-bmk-handler)))

;; handle jumping to another bookmark file
(defun nvp-bmk-handler (bmk)
  (when (> bookmark-alist-modification-count 0)
    (bookmark-save))
  (setq bookmark-default-file (bookmark-get-filename bmk))
  (nvp-bmk-update-history)
  ;; (kill-all-local-variables)
  (setq bookmark-alist nil)
  (let (bookmark-bookmarks-timestamp)
    (bookmark-maybe-load-default-file)
    ;; (bookmark-bmenu-list)
    ))

(defun nvp-bmk-make-record (filename &optional name)
  `(,(or name nil)
    ,@(bookmark-make-record-default 'no-file)
    (filename . ,(abbreviate-file-name filename))
    (handler  . nvp-bmk-handler)))

(defun nvp-bmk-cycle-previous (&optional next)
  "Cycle through bookmark history."
  (interactive)
  (when-let
      ((bmk
        (funcall (if next #'nvp-cache-next #'nvp-cache-previous) nvp-bmk-cache)))
    (unless (f-same-p bmk bookmark-default-file)
      (when (> bookmark-alist-modification-count 0)
        (bookmark-save))
      (setq bookmark-default-file bmk)
      (setq bookmark-alist nil)
      (let (bookmark-bookmarks-timestamp)
        (bookmark-maybe-load-default-file)
        ;; (bookmark-bmenu-list)
        ))))

(defun nvp-bmk-cycle-next ()
  "Cycle forward through bookmark list."
  (interactive)
  (nvp-bmk-cycle-previous 'next))

(defun nvp-bmk-create (filename &optional make-current link)
  "Create new bookmark file, prompting for FILENAME. 
(4) prefix or MAKE-CURRENT is non-nil, set new bookmark file as current 
    default bookmark file.
(16) prefix or LINK is non-nil, create link to new bookmark file from
current bookmark menu list."
  (interactive
   (list (let ((default-directory nvp-bmk-directory))
           (read-file-name "Bookmark File: ")) (nvp-prefix 4) (nvp-prefix 16)))
  (when (not (file-exists-p filename))
    (message "Creating new bookmark file at %s" filename)
    (nvp-bmk-update-history filename make-current)
    (with-temp-buffer
      (let (bookmark-alist)
        (bookmark-save nil filename))))
  (when link
    (let* ((name (read-from-minibuffer "Bookmark Link Name: "))
           (record (nvp-bmk-make-record filename)))
      (bookmark-store name (cdr record) t)))
  (when make-current
    (nvp-bmk-handler (nvp-bmk-make-record filename))))

;; FIXME: store overlays?
;; Highlight entries
(defvar-local nvp-bmk-overlays nil)

(defun nvp-bmk-toggle-highlight ()
  "Toggle highlighting of bookmark entries in *Bookmark List* buffer."
  (interactive)
  (if (setq nvp-bmk-overlays (not nvp-bmk-overlays))
      (nvp-bmk-add-overlays nvp-bmk-regexp 'nvp-bmk-bookmark-highlight)
    (nvp-bmk-remove-overlays)))

(defun nvp-bmk-add-overlays (regexp face)
  "Highlight bookmark entries in *Bookmark List* buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put overlay 'face face))
      (goto-char (match-end 0)))))

(defun nvp-bmk-remove-overlays ()
  "Remove highlighting of bookmark entries."
  (remove-overlays))


(defvar nvp-bmk-to-bmk-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c"               #'nvp-bmk-create)
    (define-key map (kbd "TAB")       #'nvp-bmk-cycle-next)
    (define-key map (kbd "C-M-n")     #'nvp-bmk-cycle-next)
    (define-key map (kbd "<backtab>") #'nvp-bmk-cycle-previous)
    (define-key map (kbd "C-M-p")     #'nvp-bmk-cycle-previous)
    (define-key map "f"               #'nvp-bmk-toggle-highlight)
    map))

;;;###autoload
(define-minor-mode nvp-bmk-to-bmk-mode
  "Toggle `nvp-bmk-to-bmk' mode.
Interactively with no arguments, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix 
argument disables it.  From lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When `nvp-bmk-to-bmk' mode is enabled, bookmark menus can be both bookmarked
and jumped between.

  \\{nvp-bmk-to-bmk-mode-map}"
  :init-value nil
  :keymap nvp-bmk-to-bmk-mode-map
  :lighter " B2B"
  (when nvp-bmk-to-bmk-mode
    (setq-local bookmark-make-record-function 'nvp-bmk-record-function)
    (nvp-bmk-toggle-highlight)
    (when (nvp-cache-empty-p nvp-bmk-cache)
      (nvp-cache-load nvp-bmk-cache))))

(provide 'nvp-bookmark)
;;; nvp-bookmark.el ends here
