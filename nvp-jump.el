;;; nvp-jump.el --- jumping places -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-04-09.23>
;; Created: 24 November 2016

;;; Commentary:

;; Functions to jump to locations
;; Prefix args should do the following:
;; 0) Default jump to other window
;; 1) With single prefix, jump same window
;; 2) With double prefix, prompt or something else
;; 3) Default action => dired location

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(require 'nvp)
(require 'nvp-read)
(nvp-decl "" nvp-scratch-minor-mode)
(nvp-decl "yasnippet" yas-expand-snippet yas-lookup-snippet)
(autoload 'nvp-file-locate-first-dominating "nvp-file")

;; -------------------------------------------------------------------
;;; Modes

;;;###autoload
(defun nvp-jump-to-mode-config (mode action)
  "Jump to MODE configuration, inserting skeleton snippet if non-existent."
  (interactive (list (nvp-read-mode-config "Jump to config: ") current-prefix-arg))
  (if (eq t mode) (dired-other-window nvp/config)
    (let ((file (nvp-mode-config-path mode)))
      (nvp-display-location
       file :file action
       :init-fn (lambda () (nvp-display-init-template
                       'mode-config 'emacs-lisp-mode nil nil
                       `(modename ,(nvp-jump--mode-name mode))))))))

;; Jump to test with extension `STR'.  If it doesn't exist make a new
;; file, and if there are multiple matches offer ido choice.
;;;###autoload
(defun nvp-jump-to-mode-test (test action)
  "Jump to TEST file for mode, creating one if necessary."
  (interactive (list (nvp-read--mode-test) current-prefix-arg))
  (nvp-display-location test :file action))

;;;###autoload
(defun nvp-jump-to-mode-hook (mode action)
  "Jump to location defining MODEs hook."
  (interactive (list (nvp-prefix 16 (nvp-read-mode) major-mode) current-prefix-arg))
  (and (stringp mode) (setq mode (intern mode)))
  (let* ((str-mode-hook (format "%s-hook" mode))
         (hook-fn-name (format "nvp-%s-hook" (nvp-jump--mode-name mode)))
         (hook-fn (intern-soft hook-fn-name)))
    (if hook-fn                         ; probably in its own config file
        (nvp-display-location hook-fn :find-func action)
      ;; otherwise goto default hook file and search for it
      (nvp-display-location nvp/hooks :file action)
      (goto-char (point-min))
      (search-forward str-mode-hook nil 'move 1))))

;; -------------------------------------------------------------------
;;; Install / build files

;;;###autoload
(defun nvp-jump-to-mode-install (file action)
  "Jump to external installation files for MODE.
With double prefix, prompt for mode."
  (interactive
   (list
    (nvp-read--mode-install (nvp-prefix 16 (nvp-read--mode-name (nvp-read-mode))))
    current-prefix-arg))
  (nvp-display-location file :file action))

;;;###autoload
(defun nvp-jump-to-build-file (file action)
  "Jump with ACTION to an init FILE in the build directory."
  (interactive
   (list (nvp-read-relative-recursively nvp/build ".el$" "Jump to init file: ")
         current-prefix-arg))
  (nvp-display-location file :file action))

;;;###autoload
(defun nvp-jump-to-nvp-keymap (keymap action)
  "Jump to one of my defined keymaps."
  (interactive (list (nvp-read-nvp-keymap) current-prefix-arg))
  (and (symbolp keymap) (setq keymap (symbol-name keymap)))
  (let ((buff (find-file-noselect
               (expand-file-name "base/nvp-bindings.el" nvp/build))))
    (with-current-buffer buff
      (goto-char (point-min))           ;might already be open
     (condition-case nil
         (when (re-search-forward
                (concat "^(nvp-bind-keys[ ]+" (regexp-quote keymap)))
           (set-marker (mark-marker) (match-end 0)))
       (error (goto-char (point-min)))))
   (nvp-display-location buff :buffer action)))

;; -------------------------------------------------------------------
;;; Org / Info

;;;###autoload
(defun nvp-jump-to-org (org-file action)
  "Jump to org file. 
If `nvp-local-notes-file' is bound use that unless there is a prefix of 16. 
Otherwise prompt, with default `nvp-default-org-file'."
  (interactive
   (list (nvp-read--org-file nil nil (nvp-prefix 16)) current-prefix-arg))
  (prog1 (setq org-file (nvp-display-location org-file :file action))
    (when (bufferp org-file)
      (with-current-buffer org-file
        (goto-char (point-min))
        (ignore-errors (search-forward "* Notes"))))))

;;;###autoload
(defun nvp-jump-to-info (file action)
  "Jump to info file (in org mode). 
With prefix jump this window, otherwise `find-file-other-window'."
  (interactive (list (nvp-read--info-files) current-prefix-arg))
  (nvp-display-location
   file :file action
   :init-fn (lambda () (nvp-display-init-template 'texinfo 'org-mode))))

;; -------------------------------------------------------------------
;;; Scratch
(autoload 'nvp-scratch-switch-modes "nvp-scratch")

;;;###autoload
(defun nvp-jump-to-scratch (mode action)
  "Jump to scratch buffer in MODE (default current `major-mode'). 
With prefix, pop other window, with double prefix, prompt for MODE."
  (interactive
   (list (nvp-prefix 16 (intern (nvp-read-mode)) major-mode) current-prefix-arg))
  (nvp-window-configuration-save)
  (let ((buff (get-buffer-create "*scratch*")))
    (with-current-buffer buff
      (nvp-display-location buff :buffer action)
      (with-current-buffer buff
        (nvp-scratch-switch-modes mode)
        (nvp-scratch-minor-mode)))))

;; -------------------------------------------------------------------
;;; Books / PDFs

;;;###autoload
(defun nvp-jump-to-book (dir &optional action)
  "Jump to book, either opening in emacs (eg. pdfs) or external for epubs.
With double prefix, prompt for directory (default `nvp-local-books-directories'
or `nvp/books'. 
With triple prefix, offer recursive results."
  (interactive
   (let* ((arg (prefix-numeric-value current-prefix-arg))
          (case-fold-search t)
          (root (cond
                  ((> arg 4)
                   (expand-file-name
                    (read-directory-name "Book Directory: " nvp/books)))
                  ((bound-and-true-p nvp-local-books-directories)
                   nvp-local-books-directories)
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
;;; Files/Directories

;; Open nearest file up the directory tree named:
;; 1. NAME if non-nil
;; 2. Prompt for file name with 2 prefix arg
;; 3. Variable `nvp-local-notes-file' (directory-local) if non-nil
;; 4. Otherwise, default to 'todo.org' or 'notes.org'.
;;;###autoload
(defun nvp-jump-to-nearest-notes-dwim (name action)
  "Jump to nearest notes/todo file, prompting with prefix."
  (interactive
   (list (or (nvp-prefix 16 (read-file-name "File name: "))
             (bound-and-true-p nvp-local-notes-file)
             '("notes.org" "Notes.org" "todo.org" "Todo.org"))
         current-prefix-arg))
  (let* ((locate-fn (if (consp name) #'nvp-file-locate-first-dominating
               #'locate-dominating-file))
         (dir (funcall locate-fn (nvp-path 'bfnd) name)))
    (if dir (nvp-display-location (expand-file-name name dir) :file action)
      (user-error (format "%S not found up the directory tree." name)))))

;;;###autoload
(defun nvp-jump-to-dotfile (dir action)
  "Jump to dotfile in other window.
With single prefix, open in this window.
With double prefix, set coding to utf-8."
  (interactive (list nvp/dots (prefix-numeric-value current-prefix-arg)))
  (let ((buff (nvp-display-location dir :ido action :find-fn #'ido-find-file-in-dir)))
    (when (eq 16 action)
      (with-current-buffer buff
        (set-buffer-file-coding-system 'utf-8-unix nil t)))))

;;;###autoload
(defun nvp-jump-to-dir (dir action)
  "Jump to some common directories."
  (interactive
   (list
    (nvp-prefix 16 (nvp-completing-read
                    (format "Directory (default %s): " nvp/scratch)
                    (list "~/" nvp/project nvp/class nvp/bin nvp/install nvp/work
                          nvp/devel nvp/modes nvp/nvp nvp/site nvp/emacs nvp/build
                          nvp/private nvp/scratch)
                    nil nil nil 'nvp-read-config-history nvp/scratch)
      nvp/scratch)
    current-prefix-arg))
  (nvp-display-location dir :ido action :find-fn #'ido-find-file-in-dir))

;;;###autoload
(defun nvp-jump-to-source (dir action)
  "Jump to local source or default devel directory."
  (interactive
   (list (cond
          ((nvp-prefix 16) "~")
          ((bound-and-true-p nvp-local-src-directories)
           (if (> 1 (length nvp-local-src-directories))
               (nvp-completing-read "Source directory: " nvp-local-src-directories
                                    nil t nil 'nvp-read-config-history)
             nvp-local-src-directories))
          (t nvp/devel))
         current-prefix-arg))
  (nvp-display-location dir :ido action :find-fn #'ido-find-file-in-dir))

;; -------------------------------------------------------------------
;;; Other

;;;###autoload
(defun nvp-jump-to-register (action)
  (interactive (list (prefix-numeric-value current-prefix-arg)))
  (nvp-display-with-action action
    (setq prefix-arg current-prefix-arg)
    (call-interactively #'jump-to-register)))

;;;###autoload
(defun nvp-jump-to-template (action)
  (interactive "P")
  (nvp-display-location nvp/template :ido action :find-fn #'ido-find-file-in-dir))

(provide 'nvp-jump)
;;; nvp-jump.el ends here
