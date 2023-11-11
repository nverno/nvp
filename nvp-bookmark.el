;;; nvp-bookmark.el --- jump b/w boomark files -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; TODO:
;; - [✓] load DWIM - check local variable, look locally, then default
;; - [✓] mode-specific bookmarks by default
;; - [✓] bookmark-to-bookmark jump handler
;; - [ ] create new bookmark files -- add prefix option to create on lookup
;; - [ ] load all bookmark files into ring from default directory
;;
;; Bmenu:
;; - [✓] link bookmark files
;; - [✓] ez way to jump b/w bookmark stashes
;; - [✓] create new bookmark w/ linkage
;; - [ ] unmark all
;; - [ ] show bookmarks for file -- maybe in annotation or something?
;; - [ ] display bookmark files as tabs to switch between
;;
;; FIXME:
;; - [✓] wrong formatting with xrefs
;; - [✓] error displayed when using jump handler in bmemu
;; - [✓] not getting added to ring
;;
;; Refs:
;; `Info-bookmark-make-record' #<marker at 212098 in info.el.gz>
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'ring)
(require 'bookmark)
(nvp:auto "f" 'f-same-p)
(nvp:decls)

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
          (delq nil (cons file nvp-bookmark-search-files))))

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

(defun nvp-bookmark-remove-duplicates ()
  "Remove bookmarks that have same \\='filename and \\='position."
  (interactive)
  (->> (seq-uniq
        bookmark-alist
        (lambda (a b)
          (and (eq (assoc-default 'position a) (assoc-default 'position b))
               (string= (assoc-default 'filename a) (assoc-default 'filename b)))))
       (setq bookmark-alist))
  (when (derived-mode-p 'bookmark-bmenu-mode)
    (revert-buffer)))


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
(nvp:setup-cache nvp-bmk-ring-name ".bmk_history")

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
  (let ((next (nvp-bmk--default-file
               (and bookmark (bookmark-get-filename bookmark))))
        (previous (unless (ring-empty-p nvp-bmk-ring)
                    (ring-ref nvp-bmk-ring 0))))
    (if previous
        (if (f-same-p next previous) nil ; just ignore if same as last
          (--if-let (ring-member nvp-bmk-ring next)
              ;; if it is already present elsewhere in the ring, move it up
              ;; to the most recent location -- head of the ring
              (ring-insert nvp-bmk-ring (ring-remove nvp-bmk-ring it))
            (ring-insert nvp-bmk-ring next))
          t)                            ; return t, an insert happened
      (ring-insert nvp-bmk-ring next)
      'first)))                         ; first insert, previous was null

(defun nvp-bmk-make-record (&optional file name)
  "Implements the `bookmark-make-record-function' type for bookmarks."
  (let* ((afile (nvp-bmk--default-file file))
         (fname (or name (file-name-nondirectory afile)))
         (bookmark-name (if fname (concat "_" fname "_"))))
    `(,bookmark-name
      ,@(bookmark-make-record-default 'no-file 'no-context 1)
      (filename . ,afile)
      (handler  . nvp-bmk-jump))))

(defun nvp-bmk-jump (bmk &optional no-insert)
  "Implements the `handler' function for the record returned by
`nvp-bmk-make-record'. This functions updates the history cache unless
NO-INSERT."
  (let* ((file (bookmark-prop-get bmk 'filename))
         (insert-p (or no-insert (nvp-bmk-ring-insert bmk))))
    (if insert-p
        (progn
          ;; could change this to prompt if `bookmark-alist-modification-count'>0
          (when (bookmark-time-to-save-p)
            (bookmark-save))
          ;; calls `bookmark-bmenu-surreptitiously-rebuild-list' which updates
          ;; the bmenu buffer already if it exists
          (bookmark-load file t nil t)
          (nvp-bmk-msg))
      (user-error "Already at bookmark '%s'?" file))))

;;; Cycling through bookmark history
(defun nvp-bmk-next-index (arg)
  "Next index in bookmark history traversal."
  (unless (ring-empty-p nvp-bmk-ring)
    (if nvp-bmk-ring-index
        (let ((sz (ring-length nvp-bmk-ring)))
          (if (> nvp-bmk-ring-index sz) ; bookmarks may have been deleted
              (1- sz)
            ;; when cycling, offset by 1 in direction of arg
            (mod (+ nvp-bmk-ring-index (if (> arg 0) 1 -1)) sz)))
      ;; start from beg. or end
      (if (>= arg 0) 0                       ; => most recent bookmark
        (1- (ring-length nvp-bmk-ring))))))  ; <= go back to oldest

;; return the next/previous bookmark and update `nvp-bmk-ring-index'
(defun nvp-bmk-next-bookmark (&optional arg)
  "Go to the next bookmark if there is one, otherwise loop back to the oldest."
  (interactive)
  (nvp:defq arg -1)
  (--if-let (nvp-bmk-next-index arg)
      (let* ((afile (ring-ref nvp-bmk-ring it))
             (bmk (nvp-bmk-make-record afile)))
        (setq nvp-bmk-ring-index it)
        ;; doesn't change ordering of entries when cycling through history
        (nvp-bmk-jump bmk 'no-insert))
    (user-error "No %s bookmark - history should be empty?"
                (if (< arg 0) "next" "previous"))))

(defun nvp-bmk-previous-bookmark ()
  "Go back to the previous bookmark, if available."
  (interactive)
  (nvp-bmk-next-bookmark 1))

;;; TODO: create new links to b/w bookmark files from bmenu
(defun nvp-bmk-link (filename)
  "Create new bookmark link from current to FILENAME."
  (interactive "fBookmark file to link: ")
  ;; XXX: ensure FILENAME is actually a bookmark file
  (pcase-let ((`(,str . ,alist) (nvp-bmk-make-record filename)))
    (bookmark-store str alist t)))

;; (defun nvp-bmk-create (filename &optional make-current link)
;;   "Create new bookmark file, prompting for FILENAME. 
;; (4) prefix or MAKE-CURRENT is non-nil, set new bookmark file as current 
;;     default bookmark file.
;; (16) prefix or LINK is non-nil, create link to new bookmark file from
;; current bookmark menu list."
;;   (interactive
;;    (list (let ((default-directory nvp-bmk-directory))
;;            (read-file-name "Bookmark File: ")) (nvp:prefix 4) (nvp:prefix 16)))
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

;; FIXME: overlays sucx
;; Highlight entries
(defvar nvp-bmk-regexp "_.*\\.bmk_$" "Regexp to match bookmark entries.")

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
  (remove-overlays (point-min) (point-max) 'face 'nvp-bmk-bookmark-highlight))

(defvar nvp-bmk-to-bmk-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map "c"               #'nvp-bmk-create)
    (define-key map (kbd "<tab>")     #'nvp-bmk-next-bookmark)
    (define-key map (kbd "C-M-n")     #'nvp-bmk-next-bookmark)
    (define-key map (kbd "<backtab>") #'nvp-bmk-previous-bookmark)
    (define-key map (kbd "C-M-p")     #'nvp-bmk-previous-bookmark)
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
  (if nvp-bmk-to-bmk-mode
      (progn
        ;; TODO: load cache with known bookmarks
        (setq-local bookmark-make-record-function 'nvp-bmk-make-record)
        ;; tries to get prop on entry that may no longer be in the bookmark-alist
        (setq-local bookmark-automatically-show-annotations nil)
        (nvp-bmk-toggle-highlight))
    (nvp-bmk-remove-overlays)))

(provide 'nvp-bookmark)
;;; nvp-bookmark.el ends here
