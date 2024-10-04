;;; nvp-bookmark.el --- bookmarks -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'bookmark)
(nvp:decls)

(defconst nvp-bookmark-directory
  (expand-file-name "bookmarks" nvp/cache))

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
  ;; Try expanding FILE against `nvp-bookmark-search-roots' to find existing
  ;; file
  (--some
   (--when-let (expand-file-name file (if (functionp it) (funcall it file) it))
     (and (file-exists-p it) it))
   nvp-bookmark-search-roots))

(defun nvp-bookmark-locate-file (&optional file)
  "Locate bookmark file by ordering in `nvp-bookmark-search-files', searching
in `nvp-bookmark-search-roots' when an entry doesn't exist."
  (--some (when (or (and (symbolp it) (boundp it)
                         (setq it (symbol-value it)))
                    it)
            (if (and (stringp it) (file-exists-p it))
                it ; absolute path given
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
  (let ((cur-len (length bookmark-alist)))
    (->> (seq-uniq
          bookmark-alist
          (lambda (a b)
            (and (eq (assoc-default 'position a)
                     (assoc-default 'position b))
                 (string= (assoc-default 'filename a)
                          (assoc-default 'filename b)))))
         (setq bookmark-alist))
    (let ((new-len (length bookmark-alist)))
      (when (and (< new-len cur-len)
                 (derived-mode-p 'bookmark-bmenu-mode))
        (revert-buffer))
      (message "removed %d bookmarks" (- cur-len new-len)))))

(provide 'nvp-bookmark)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-bookmark.el ends here
