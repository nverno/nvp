;;; nvp-jump.el --- jumping places -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Functions to jump to locations
;;
;; Prefix args should do the following:
;; 0) Default jump to other window
;; 1) With single prefix, jump same window
;; 2) With double prefix, prompt or something else
;; 3) Default action => dired location
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:req 'nvp-read 'subrs)
(require 'nvp)
(require 'nvp-display)
(require 'nvp-read)
(nvp:auto "lisp-mnt" 'lm-header)
(nvp:auto "find-func" 'find-function-library 'find-library-name)
(nvp:auto "nvp-scratch" 'nvp-scratch-switch-modes)
(nvp:decls :v (ido-default-buffer-method))

;; provides ido-completion for things like `locate-library', although it
;; is just noticeably slower for some functions -- especially those related
;; to reading libraries/obarray
;; Might consider blacklisting `locate-library' -- although having the
;; ido-vertical support there is dope
(nvp:maybe-enable 'ido-ubiquitous-mode 'ido-completing-read+)

;; -------------------------------------------------------------------
;;; Emacs Libraries

;;;###autoload
(defun nvp-jump-to-library (library &optional action)
  "Jump to LIBRARY file."
  (interactive (list (call-interactively 'locate-library) current-prefix-arg))
  (when library
    (let* ((el (concat (file-name-sans-extension library) ".el"))
           (elgz (concat el ".gz")))
      (nvp-display-location (if (file-exists-p el) el elgz) :file action))))

(defun nvp--get-library-file (&optional lisp-only prompt)
  "Get defining file for current symbol or prompt for library.
Optionally, search LISP-ONLY files (no C sources)."
  (let* ((load-suffixes (if lisp-only '(".el")))
         (curr (symbol-at-point))
         (sym (if (or prompt (null curr)) (read-library-name) curr))
         (lib (if (and (symbolp sym) (not (memq sym features)))
                  (cdr (find-function-library sym lisp-only))
                (if (file-name-absolute-p sym) sym
                  (locate-file sym
                               load-path
                               (append (get-load-suffixes)
                                       load-file-rep-suffixes))))))
    lib))

(defun nvp-jump--git-url ()
  (let ((str (shell-command-to-string "git remote get-url origin")))
    (--when-let (and (string-match
                      "\\(?:git@github.com:\\|https://github.com/\\)\\([^ ]+\\)" str)
                     (string-trim-right (match-string 1 str)))
      (concat "https://github.com/" it))))

;;; TODO: make more generic
;;;###autoload
(defun nvp-jump-to-library-url (&optional choose)
  "Browse URL of either file defining symbol at point or prompt for library.
(1) prefix or CHOOSE, prompt for library."
  (interactive "P")
  (let (url)
    (cond
     ((and (not choose) (memq major-mode '(shell-mode eshell-mode)))
      (--if-let (nvp-jump--git-url)
          (browse-url it)
        (user-error "No git repo here")))
     ((and (not choose)
           (eq major-mode 'emacs-lisp-mode)
           (setq url (save-excursion (or (lm-header "URL")
                                         (lm-header "Homepage")))))
      (browse-url url))                ; found URL in current buffer!!
     (t ;; no URL in emacs sources
      (--if-let (and (not choose) (nvp-jump--git-url))
          (browse-url it)
        (--when-let (nvp--get-library-file 'lisp-only choose)
          (if (not (string= (file-name-extension it) "el"))
              (user-error "Library %S isn't elisp." it)
            (with-temp-buffer
              (insert-file-contents it)
              (if (setq url (lm-header "URL"))
                  (browse-url url)
                (user-error "Library %S has no URL header" it))))))))))

(defun nvp--locate-library-source (library)
  (let* ((name (file-name-nondirectory library))
         (locs
          (split-string
           (shell-command-to-string
            (format
             "find %s -type f -name %s.el"
             (or source-directory
                 (expand-file-name "emacs" (getenv "DEVEL")))
             name))
           nil t " ")))
    (when locs
      (if (> (length locs) 1)
          (nvp-completing-read "Source file: " locs)
        (car locs)))))

;;;###autoload
(defun nvp-jump-to-emacs-source (library &optional action)
  "Jump to git source for emacs builtin library."
  (interactive (list (call-interactively #'locate-library) current-prefix-arg))
  (setq library (file-name-sans-extension library))
  (-if-let (src (nvp--locate-library-source library))
      (nvp-display-location src :file action)
    (user-error "No source found for %s" library)))


;; -------------------------------------------------------------------
;;; Modes

;;;###autoload
(defun nvp-jump-to-mode-config (mode action)
  "Jump to MODE configuration, inserting skeleton snippet if non-existent."
  (interactive (list (nvp-read-mode-config "Jump to config: ") current-prefix-arg))
  (if (eq t mode) (dired-other-window nvp/config)
    (let ((file (nvp:mode-config-path mode)))
      (nvp-display-location
       file :file action
       :init-fn (lambda () (nvp-display-init-template
                       'mode-config 'emacs-lisp-mode nil nil
                       `(modename ,(nvp:read-mode-name mode))))))))

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
  (interactive (list (nvp:prefix 16 (nvp-read-mode) major-mode) current-prefix-arg))
  (and (stringp mode) (setq mode (intern mode)))
  (let* ((str-mode-hook (format "%s-hook" mode))
         (hook-fn-name (format "nvp-%s-hook" (nvp:read-mode-name mode)))
         (hook-fn (intern-soft hook-fn-name)))
    (if hook-fn                         ; probably in its own config file
        (nvp-display-location hook-fn :find-func action)
      ;; otherwise goto default hook file and search for it
      (nvp-display-location nvp/hooks :file action)
      (goto-char (point-min))
      (search-forward str-mode-hook nil 'move 1))))

;;;###autoload
(defun nvp-jump-to-mode-lib (action)
  "Jump to mode LIB addons with ACTION."
  (interactive "p")
  (nvp-jump-to-dir nvp/modes action))


;; -------------------------------------------------------------------
;;; Install / build files

;;;###autoload
(defun nvp-jump-to-installer (file action)
  "Jump to external installation files."
  (interactive
   (list
    (nvp-read-relative-recursively nvp/bin "^[^.]+$" "Install file: ")
    current-prefix-arg))
  (nvp-display-location file :file action))

;;;###autoload
(defun nvp-jump-to-build-file (file action)
  "Jump with ACTION to an init FILE in the build directory."
  (interactive
   (list (nvp-read-relative-recursively
          nvp/build "\\(?:.el\\|/\\)$" "Jump to init file: ")
         current-prefix-arg))
  (nvp-display-location file :file action))


;; -------------------------------------------------------------------
;;; Files/Directories

;; Open nearest file up the directory tree named:
;; 1. NAME if non-nil
;; 2. Prompt for file name with 2 prefix arg
;; 3. Variable `nvp-local-notes-file' (directory-local) if non-nil
;; 4. Otherwise, default to 'todo.org' or 'notes.org'.
;;;###autoload
(defun nvp-jump-to-nearest-notes-dwim (&optional name action)
  "Jump to nearest notes/todo file, prompting with prefix."
  (interactive
   (list (nvp:prefix '>4
           (prog1 (read-file-name "File name: ")
             (setq current-prefix-arg '(1))))
         current-prefix-arg))
  (--if-let (nvp:find-notes-file name) (nvp-display-location it :file action)
    (user-error (format "%S not found up the directory tree." name))))

;;;###autoload
(defun nvp-jump-to-dotfile (dir action)
  "Jump to dotfile in other window."
  (interactive (list (nvp-read-relative-recursively nvp/dots "") current-prefix-arg))
  (nvp-display-location dir :file action))

;;;###autoload
(defun nvp-jump-to-dir (dir action)
  "Jump to some common directories."
  (interactive
   (list
    (nvp:prefix 16 (nvp-read-relative-recursively nvp/scratch)
      (nvp-completing-read "Directory: " nvp-default-directories
        nil nil nil 'nvp-read-config-history))
    current-prefix-arg))
  (nvp-display-location dir :file action))

;;;###autoload
(defun nvp-jump-to-source (dir action)
  "Jump to local source or default devel directory."
  (interactive
   (list (cond
          ((nvp:prefix 16) "~")
          ((bound-and-true-p nvp-local-src-directories)
           (if (> 1 (length nvp-local-src-directories))
               (nvp-completing-read "Source directory: " nvp-local-src-directories
                                    nil t nil 'nvp-read-config-history)
             nvp-local-src-directories))
          (t nvp/devel))
         current-prefix-arg))
  (let ((file (nvp-read-relative-recursively dir)))
    (nvp-display-location file :file action)))

;;;###autoload
(defun nvp-jump-to-org (org-file action)
  "Jump to org file. 
If `nvp-local-notes-file' is bound use that unless there is a prefix of 16. 
Otherwise prompt, with default `nvp-default-org-file'."
  (interactive
   (list (nvp-read--org-file nil nil (nvp:prefix 16))
         (nvp:prefix '>=16 1 current-prefix-arg)))
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

;;;###autoload
(defun nvp-jump-to-template (action)
  "Jump to a project template."
  (interactive "P")
  (nvp-display-location
   (nvp-read-relative-recursively nvp/template) :file action))


;; -------------------------------------------------------------------
;;; Other: keymaps, register, scratch

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
                (concat "^(nvp:bindings[ ]+" (regexp-quote keymap)))
           (set-marker (mark-marker) (match-end 0)))
       (error (goto-char (point-min)))))
   (nvp-display-location buff :buffer action)))

;;;###autoload
(defun nvp-jump-to-register (action)
  (interactive (list (prefix-numeric-value current-prefix-arg)))
  (nvp:display-with-action action
    (setq prefix-arg current-prefix-arg)
    (call-interactively #'jump-to-register)))

;;;###autoload
(defun nvp-jump-to-scratch (mode action)
  "Jump to scratch buffer in MODE (default current `major-mode'). 
With prefix, pop other window, with double prefix, prompt for MODE."
  (interactive
   (list (nvp:prefix 16 (intern (nvp-read-mode)) major-mode) current-prefix-arg))
  (nvp-window-configuration-save)
  (let ((buff (get-buffer-create "*scratch*"))
        (default-directory nvp/scratch))
    (with-current-buffer buff
      (nvp-scratch-switch-modes mode 'activate)
      (nvp-display-location buff :buffer action))))

(provide 'nvp-jump)
;;; nvp-jump.el ends here
