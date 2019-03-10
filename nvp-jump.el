;;; nvp-jump.el --- jumping places -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-03-09 19:01:26>
;; Created: 24 November 2016

;;; Commentary:

;; Functions to jump to locations
;; Prefix args should do the following:
;; 0) Default jump to other window
;; 1) With single prefix, jump same window
;; 2) With double prefix, prompt or something else

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro)
  (nvp-local-vars))
(require 'nvp)
(require 'nvp-read)
(nvp-declare "yasnippet" yas-expand-snippet yas-lookup-snippet)
(autoload 'nvp-file-locate-first-dominating "nvp-file")

;; -------------------------------------------------------------------
;;; Modes

;;;###autoload
(defun nvp-jump-to-mode-config (mode action)
  "Jump to MODE configuration, inserting skeleton snippet if non-existent."
  (interactive (list (nvp-read-mode-config "Jump to config: ") current-prefix-arg))
  (if (eq t mode) (dired-other-window nvp/config)
    (let ((file (nvp-mode-config-path mode)))
      (nvp-display-location file :file action)
      (unless (file-exists-p file)
        (yas-expand-snippet
         (yas-lookup-snippet 'mode-config) nil nil
         `((modename ,(if (string-suffix-p "-mode" mode) (substring mode -5)
                        mode))))))))

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
  (interactive
   (list (if (eq 16 (prefix-numeric-value current-prefix-arg)) (nvp-read-mode)
           major-mode)
         current-prefix-arg))
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
                      (if (eq 16 (prefix-numeric-value current-prefix-arg))
                          (substring (nvp-read-mode) 0 -5)))
                     current-prefix-arg))
  (nvp-display-location file :file action))

;;;###autoload
(defun nvp-jump-to-build-file (file action)
  "Jump with ACTION to an init FILE in the build directory."
  (interactive
   (list (nvp-read-relative-recursively
          nvp-build-init-dir ".el$" "Jump to init file: ")
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
   (list (nvp-read--org-file
          nil nil (eq 16 (prefix-numeric-value current-prefix-arg)))
         current-prefix-arg))
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
  (nvp-display-location file :file action))

;; -------------------------------------------------------------------
;;; Scratch

;;;###autoload
(defun nvp-jump-to-scratch (mode action)
  "Jump to scratch buffer in MODE (default current `major-mode'). 
With prefix, pop other window, with double prefix, prompt for MODE."
  (interactive
   (list (if (eq 16 (prefix-numeric-value current-prefix-arg))
             (intern (nvp-read-mode))
           major-mode)
         current-prefix-arg))
  (let ((buff (get-buffer-create "*scratch*")))
    (with-current-buffer buff
      (if (provided-mode-derived-p mode 'emacs-lisp-mode)
          (unless (eq major-mode 'lisp-interaction-mode)
            (lisp-interaction-mode))
        (when (provided-mode-derived-p mode 'comint-mode)
          (setq mode 'sh-mode))
        (let ((inhibit-read-only t))
          (kill-all-local-variables)
          (erase-buffer)
          (funcall mode)))
      (nvp-display-location buff :buffer action)
      (with-current-buffer (current-buffer)
        (nvp-use-local-keymap :keymap (current-local-map)
         ("C-c C-k" . kill-this-buffer)
         ("C-c C-s" . (lambda () (interactive)
                        (funcall (intern (nvp-read-mode)))))))
      (nvp-msg "Press \\[kill-this-buffer] to kill this buffer \
or C-c C-s to switch major modes. " :keys t))))

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
   (list (or (and (eq 16 (prefix-numeric-value current-prefix-arg))
                  (read-file-name "File name: "))
             (bound-and-true-p nvp-local-notes-file)
             '("notes.org" "Notes.org" "todo.org" "Todo.org"))
         current-prefix-arg))
  (let* ((locate-fn (if (consp name) #'nvp-file-locate-first-dominating
               #'locate-dominating-file))
         (dir (funcall locate-fn (or (buffer-file-name)
                                     default-directory)
                       name)))
    (if dir (nvp-display-location (expand-file-name name dir) :file action)
      (user-error (format "%S not found up the directory tree." name)))))

;;;###autoload
(defun nvp-jump-to-dotfile (dir action)
  "Jump to dotfile in other window.
With single prefix, open in this window.
With double prefix, set coding to utf-8."
  (interactive (list nvp/dots current-prefix-arg))
  (let ((buff (nvp-display-location dir :ido action #'ido-find-file-in-dir)))
    (when (eq 16 action)
      (with-current-buffer buff
        (set-buffer-file-coding-system 'utf-8-unix nil t)))))

;;;###autoload
(defun nvp-jump-to-dir (dir action)
  (interactive
   (list (if (eq (prefix-numeric-value current-prefix-arg) 16) nvp/project nvp/class)
         current-prefix-arg))
  (nvp-display-location dir :ido action #'ido-find-file-in-dir))

;;;###autoload
(defun nvp-jump-to-source (dir action)
  "Jump to local source or default devel directory."
  (interactive
   (list (cond
          ((eq 16 (prefix-numeric-value current-prefix-arg)) "~")
          ((bound-and-true-p nvp-local-src-directories)
           (if (> 1 (length nvp-local-src-directories))
               (nvp-completing-read "Source directory: " nvp-local-src-directories
                                    nil t nil 'nvp-read-config-history)
             nvp-local-src-directories))
          (t nvp/devel))
         current-prefix-arg))
  (nvp-display-location dir :ido action #'ido-find-file-in-dir))

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
  (nvp-display-location nvp/template :ido action #'ido-find-file-in-dir))

(provide 'nvp-jump)
;;; nvp-jump.el ends here
