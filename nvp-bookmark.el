;;; nvp-bookmark.el --- jump b/w boomark files -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Want:
;; - [✓] mode-specific bookmarks by default
;; - [ ] ez way to jump b/w bookmark stashes
;; - [ ] bookmark-to-bookmark jump handler
;; Functions:
;; - [✓] load DWIM - check local variable, look locally, then default
;; - [ ] link bookmark files
;;
;; Bmenu:
;; - [ ] unmark all
;; - [ ] create new bookmark w/ linkage
;;
;; `Info-bookmark-make-record' #<marker at 212098 in info.el.gz>
;;
;; FIXME:
;; - wrong formatting with xrefs
;;
;; TODO:
;; - create new bookmark files
;; - interface to list bookmark files
;; - show bookmarks for a given file
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'ring)
(require 'bookmark)
(nvp-auto "f" 'f-same-p)
(nvp-decls)

(defconst nvp-bookmark-directory (expand-file-name "bookmarks" nvp/cache))

(defvar nvp-bookmark-search-files
  '(nvp-local-bookmark-file "bookmarks.el" bookmark-default-file)
  "The first existing file is found by the order in this list.
If an entry isn't an existing path, it will be expanded against 
`nvp-bookmark-search-roots', returning the first existing file.")

(defvar nvp-bookmark-search-roots
  '((lambda (f) (locate-dominating-file f default-directory))
    (lambda (_f) (nvp-project-root))
    nvp-bookmark-directory)
  "Order of locations from which to search for the bookmark file.
Entries can be functions, called with a single file argument.  The first file
found is returned. Default order (1) `locate-dominating-file', 
(2) `nvp-project-root', (3) in `nvp-bookmark-directory'.")

(defun nvp-bookmark-locate-file-1 (file)
  ;; try expanding FILE against `nvp-bookmark-search-roots' to find existing file
  (--some
   (--when-let (expand-file-name file (if (functionp it) (funcall it file) it))
     (and (file-exists-p it) it))
   nvp-bookmark-search-roots))

(defun nvp-bookmark-locate-file (&optional file)
  "Locate bookmark file by ordering in `nvp-bookmark-search-files', searching
in `nvp-bookmark-search-roots' when an entry doesn't exist."
  (--some (when (or (and (symbolp it) (boundp it) (setq it (symbol-value it)))
                    it)
            (if (and (stringp it) (file-exists-p it)) it   ; absolute path given
              (nvp-bookmark-locate-file-1 it)))
          (delq nil (cons file nvp-bookmark-search-list))))

;;;###autoload
(defun nvp-bookmark-load (&optional no-default)
  "Load bookmarks using `nvp-bookmark-locate-file' to determine default.
With prefix NO-DEFAULT, doesn't set new bookmarks as defaults (opposite 
behaviour of default)."
  (interactive "P")
  (-if-let (file (nvp-bookmark-locate-file))
      (bookmark-load file (not no-default) nil (not no-default))
    (funcall-interactively #'bookmark-load))
  (call-interactively #'bookmark-bmenu-list))



;; -------------------------------------------------------------------
;;; Bookmark <=> Bookmark handling

(defgroup nvp-bmk nil
  "Manage jumping between/bookmarking multiple bookmark files."
  :group 'bookmark
  :prefix "nvp-bmk-")

(defun nvp-bookmark--sync ()
  (cl-incf bookmark-alist-modification-count)
  (when (bookmark-time-to-save-p)
    (bookmark-save))
  (bookmark-bmenu-surreptitiously-rebuild-list))

(defface nvp-bmk-bookmark-highlight
  '((((background dark)) (:background "light blue" :foreground "black"))
    (t (:background "light blue")))
  "Face to highlight bookmark entries."
  :group 'nvp-bmk)

(defvar nvp-bmk-verbose t "Print messages")

;; `ring-insert' => newest item, `ring-remove' => oldest item
(defvar nvp-bmk-ring (make-ring 65) "Bookmark history list.")
(defvar nvp-bmk-ring-index nil)
(nvp-setup-cache nvp-bmk-ring-name ".bmk_history")

(defsubst nvp-bmk--default-file (&optional file)
  (abbreviate-file-name
   (or file (car bookmark-bookmarks-timestamp)
       (expand-file-name bookmark-default-file))))

(defun nvp-bmk-msg (&optional format &rest args)
  (when nvp-bmk-verbose
    (if format (message format args)
      (message "Current bookmark: %s" (nvp-bmk--default-file)))))

(defun nvp-bmk-ring-insert (&optional bookmark)
  ;; insert BOOKMARK into history ring, growing when necessary
  ;; entries are just abbreviated bookmark filenames
  ;; return non-nil if an insertion was made, 'first if there was no previous
  ;; element in the ring
  (let ((default (nvp-bmk--default-file
                  (and bookmark (bookmark-get-filename bookmark))))
        (previous (unless (ring-empty-p nvp-bmk-ring)
                    (ring-ref nvp-bmk-ring 0))))
    (if (and previous (f-same-p default previous)) nil
      (ring-insert+extend nvp-bmk-ring default 'grow)
      (if (null previous) 'first t))))

(defun nvp-bmk-make-record (&optional file name)
  "Implements the `bookmark-make-record-function' type for bookmarks."
  (let* ((afile (nvp-bmk--default-file file))
         (fname (or name (file-name-nondirectory afile)))
         (bookmark-name (if fname (concat "_" fname "_")))
         (prev (unless (ring-empty-p nvp-bmk-ring)
                 (ring-ref nvp-bmk-ring 0)))
         (defaults
           (delq
            nil
            (list bookmark-name
                  afile
                  nvp-bmk-ring-name     ; cached ring filename
                  prev))))              ; previous bookmark, or nil
    `(,bookmark-name
      ,@(bookmark-make-record-default 'no-file 'no-context 1)
      (filename . ,afile)
      (handler  . nvp-bmk-jump)
      (defaults . ,defaults))))

(defun nvp-bmk-jump (bmk)
  "Implements the `handler' function for the record returned by 
`nvp-bmk-make-record'."
  (let ((file (bookmark-prop-get bmk 'filename))
        (insert-p (nvp-bmk-ring-insert)))
    (if insert-p
        (let ((buf (save-window-excursion
                     (bookmark-load file t nil t)
                     (bookmark-bmenu-list)
                     (current-buffer))))
          (nvp-bmk-msg)
          ;; default bookmark handler to move to location
          (bookmark-default-handler
           `("" (buffer . ,buf) . ,(bookmark-get-bookmark-record bmk))))
      (user-error "Already at bookmark '%s'?" file))))

;; (defvar nvp-bmk-cache
;;   (nvp-cache-create 'ring
;;     :filename ".bmk_history"
;;     :insert-filter #'abbreviate-file-name
;;     :save-predicate #'file-exists-p)
;;   "Cache bookmark lists.")

;; Create bookmark record for bookmark-menu-list from current default
;; (defun nvp-bmk-record-function ()
;;   `((filename . ,(bookmark-buffer-file-name))
;;     (handler  . nvp-bmk-handler)))

;; (defun nvp-bmk-next-index (arg)
;;   "Get next index in ring based on direction."
;;   (if (and r-l)))

(defun nvp-bmk-cycle-start (arg)
  "Index to start a bookmark history traversal."
  (if nvp-bmk-ring-index
      ;; if a search is running, offset by 1 in direction of arg
      (mod (+ nvp-bmk-ring-index (if (> arg 0) 1 -1))
           (ring-length nvp-bmk-ring-insert))
    ;; New search, start from beg. or end
    (if (>= arg 0)
        0                               ; First elt for forward
      (1- (ring-length nvp-bmk-ring-insert)))))

(defun nvp-bmk-cycle-previous (&optional next)
  "Cycle through bookmark history."
  (interactive)
  (--when-let (funcall (if next #'ring-plus1 #'ring-minus1)))
  (when-let
      ((bmk
        (funcall (if next #'nvp- #'nvp-cache-previous) nvp-bmk-cache)))
    (unless (f-same-p bmk bookmark-default-file)
      (when (> bookmark-alist-modification-count 0)
        (bookmark-save))
      (setq bookmark-default-file bmk)
      (setq bookmark-alist nil)
      (let (bookmark-bookmarks-timestamp)
        (bookmark-maybe-load-default-file)
        ;; (bookmark-bmenu-list)
        ))))

;; (defun nvp-bmk-cycle-next ()
;;   "Cycle forward through bookmark list."
;;   (interactive)
;;   (nvp-bmk-cycle-previous 'next))

;; (defun nvp-bmk-create (filename &optional make-current link)
;;   "Create new bookmark file, prompting for FILENAME. 
;; (4) prefix or MAKE-CURRENT is non-nil, set new bookmark file as current 
;;     default bookmark file.
;; (16) prefix or LINK is non-nil, create link to new bookmark file from
;; current bookmark menu list."
;;   (interactive
;;    (list (let ((default-directory nvp-bmk-directory))
;;            (read-file-name "Bookmark File: ")) (nvp-prefix 4) (nvp-prefix 16)))
;;   (when (not (file-exists-p filename))
;;     (message "Creating new bookmark file at %s" filename)
;;     (nvp-bmk-update-history filename make-current)
;;     (with-temp-buffer
;;       (let (bookmark-alist)
;;         (bookmark-save nil filename))))
;;   (when link
;;     (let* ((name (read-from-minibuffer "Bookmark Link Name: "))
;;            (record (nvp-bmk-make-record filename)))
;;       (bookmark-store name (cdr record) t)))
;;   (when make-current
;;     (nvp-bmk-handler (nvp-bmk-make-record filename))))

;; FIXME: store overlays?
;; Highlight entries
(defvar nvp-bmk-regexp "^.*\\.bmk$" "Regexp to match bookmark entries.")

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
    ;; (define-key map "c"               #'nvp-bmk-create)
    ;; (define-key map (kbd "TAB")       #'nvp-bmk-cycle-next)
    ;; (define-key map (kbd "C-M-n")     #'nvp-bmk-cycle-next)
    ;; (define-key map (kbd "<backtab>") #'nvp-bmk-cycle-previous)
    ;; (define-key map (kbd "C-M-p")     #'nvp-bmk-cycle-previous)
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
    (setq-local bookmark-make-record-function 'nvp-bmk-make-record)
    (nvp-bmk-toggle-highlight)
    ;; (when (nvp-cache-empty-p nvp-bmk-cache)
    ;;   (nvp-cache-load nvp-bmk-cache))
    ))

(provide 'nvp-bookmark)
;;; nvp-bookmark.el ends here
