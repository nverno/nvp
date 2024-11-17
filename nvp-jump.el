;;; nvp-jump.el --- jumping places -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-read)
(nvp:decls)

(autoload 'lm-header "lisp-mnt")
(autoload 'find-function-library "find-func")
(autoload 'nvp-scratch-switch-modes "nvp-scratch")

;; Provides ido-completion for things like `locate-library', although it
;; is just noticeably slower for some functions -- especially those related
;; to reading libraries/obarray
;; Might consider blacklisting `locate-library' -- although having the
;; ido-vertical support there is dope
(when (bound-and-true-p nvp-enable-ido)
  (nvp:maybe-enable 'ido-ubiquitous-mode 'ido-completing-read+))


(defun nvp-display-init-template (template &optional mode start end &rest bindings)
  "Use TEMPLATE to init a new file.
MODE and BINDINGS are passed to `yas-expand-snippet'."
  (cl-progv (mapcar #'car bindings) (mapcar #'cadr bindings)
    (yas-expand-snippet
     (yas-lookup-snippet template mode) start end)))

;;;###autoload
(defun nvp-jump-to-library (library &optional prefix)
  "Jump to LIBRARY file according to PREFIX."
  (interactive (list (call-interactively 'locate-library) current-prefix-arg))
  (when library
    (let* ((el (concat (file-name-sans-extension library) ".el"))
           (elgz (concat el ".gz"))
           (buf (find-file-noselect (if (file-exists-p el) el elgz))))
      (nvp-with-display-actions prefix
        :action-order '(other same frame)
        (pop-to-buffer buf)))))

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
                      "\\(?:git@github.com:\\|https://github.com/\\)\\([^ ]+\\)"
                      str)
                     (string-trim-right (match-string 1 str)))
      (concat "https://github.com/" it))))

;;;###autoload
(defun nvp-jump-to-library-url (&optional choose)
  "Browse URL of either file defining symbol at point or prompt for library.
(1) prefix or CHOOSE, prompt for library."
  (interactive "P")
  (let (url)
    (cond ((and (not choose)
                (memq major-mode '(shell-mode eshell-mode)))
           (or (setq url (nvp-jump--git-url))
               (user-error "No git repo here")))
          ((and (not choose)
                (eq major-mode 'emacs-lisp-mode)
                ;; found URL in current buffer
                (setq url (save-excursion
                            (or (lm-header "URL")
                                (lm-header "Homepage"))))))
          ((and (not choose) (setq url (nvp-jump--git-url))))
          (t (--when-let (nvp--get-library-file 'lisp-only choose)
               (if (not (string= (file-name-extension it) "el"))
                   (user-error "Library %S isn't elisp." it)
                 (with-temp-buffer
                   (insert-file-contents it)
                   (or (setq url (lm-header "URL"))
                       (user-error "Library %S has no URL header" it)))))))
    (browse-url url)))

(defun nvp--locate-library-source (library)
  (let* ((name (file-name-nondirectory library))
         (locs (split-string
                (shell-command-to-string
                 (format "find %s -type f -name %s.el"
                         (or source-directory
                             (expand-file-name "emacs" (getenv "DEVEL")))
                         name))
                nil t " ")))
    (when locs
      (if (> (length locs) 1)
          (nvp-completing-read "Source file: " locs)
        (car locs)))))

;;;###autoload
(defun nvp-jump-to-emacs-source (library &optional prefix)
  "Jump to git source for emacs builtin library."
  (interactive (list (call-interactively #'locate-library) current-prefix-arg))
  (setq library (file-name-sans-extension library))
  (-if-let (src (nvp--locate-library-source library))
      (nvp-with-display-actions prefix :action-order '(other same frame)
        (pop-to-buffer (find-file-noselect src)))
    (user-error "No source found for %s" library)))

;;;###autoload
(defun nvp-jump-to-mode-config (mode action)
  "Jump to MODE configuration, inserting skeleton snippet if non-existent."
  (interactive (list (let ((mode (nvp-read-mode-config "Jump to config: ")))
                       (if (equal "" mode) t mode))
                     current-prefix-arg))
  (nvp-with-display-actions action :action-order '(other same frame)
    (if (eq t mode) (dired nvp/config)
      (let ((buf (find-file-noselect (nvp:mode-config-path mode))))
        (pop-to-buffer buf)
        (unless (file-exists-p buffer-file-name)
          (nvp-display-init-template
           'mode-config 'emacs-lisp-mode nil nil
           `(modename ,(nvp:mode-name mode))))))))

;;;###autoload
(defun nvp-jump-to-mode-test (test action)
  "Jump to TEST file for mode, creating one if necessary."
  (interactive (list (nvp-read--mode-test) current-prefix-arg))
  (nvp-with-display-actions action :action-order '(other same frame)
    (pop-to-buffer (find-file-noselect test))))

;;;###autoload
(defun nvp-jump-to-mode-hook (action)
  "Jump to location defining mode hook."
  (interactive "P")
  (nvp-with-display-actions action
    (let* ((mode (if current-prefix-arg
                     (intern (nvp-read-mode "Jump to mode hook: "))
                   (nvp:mode t)))
           (str-mode-hook (format "%s-hook" mode))
           (hook-fn-name (format "nvp-%s-hook" (nvp:mode-name mode)))
           (hook-fn (intern-soft hook-fn-name)))
      (if hook-fn                       ; probably in its own config file
          (find-function-do-it hook-fn nil #'pop-to-buffer)
        ;; Otherwise goto default hook file and search for it
        (with-current-buffer (find-file-noselect nvp/hooks)
          (goto-char (point-min))
          (search-forward str-mode-hook nil 'move 1)
          (pop-to-buffer (current-buffer)))))))

;;;###autoload
(defun nvp-jump-to-mode-lib (action)
  "Jump to mode LIB addons with ACTION."
  (interactive "P")
  (nvp-jump-to-frecent-directory action nvp/modes))


;; -------------------------------------------------------------------
;;; Install / build files

;;;###autoload
(defun nvp-jump-to-installer (action)
  "Jump to external installation files."
  (interactive "P")
  (let* ((file (nvp-read-relative-recursively
                nvp/bin "^[^.]+$" "Install file: "))
         (buf (find-file-noselect file)))
    (nvp-with-display-actions action :action-order '(other same frame)
      (pop-to-buffer buf))))

;;;###autoload
(defun nvp-jump-to-build-file (action)
  "Jump with ACTION to an init FILE in the build directory."
  (interactive "P")
  (let* ((file (nvp-read-relative-recursively
                nvp/build "\\(?:.el\\|/\\)$" "Jump to init file: "))
         (buf (find-file-noselect file)))
    (nvp-with-display-actions action :action-order '(other same frame)
      (pop-to-buffer buf))))


;; -------------------------------------------------------------------
;;; Files/Directories

;; Open nearest file up the directory tree named:
;; 1. NAME if non-nil
;; 2. Prompt for file name with 2 prefix arg
;; 3. Variable `nvp-local-notes-file' (directory-local) if non-nil
;; 4. Otherwise, default to 'todo.org' or 'notes.org'.
;;;###autoload
(defun nvp-jump-to-nearest-notes-dwim (&optional action names)
  "Jump to nearest notes/todo file matching NAMES according to ACTION.
With extra prefix, prompt for file name."
  (interactive "P")
  (nvp-with-display-actions action :action-order '(other same frame)
    :other-action '((nvp-display-buffer-in-left-side-window)
                    (dedicated . t))
    (and current-prefix-arg
         (setq names (read-file-name "Match notes name: " nil nil nil names)))
    (let* ((file (nvp:find-notes-file names))
           (buf (and file (find-file-noselect file))))
      (if buf (pop-to-buffer buf)
        (user-error (format "%S not found up the directory tree."
                            (or names "No notes")))))))

;;;###autoload
(defun nvp-jump-to-dotfile (action)
  "Jump to dotfile in other window."
  (interactive "P")
  (nvp-with-display-actions action :action-order '(other same frame)
    (pop-to-buffer
     (find-file-noselect (nvp-read-relative-recursively nvp/dots "")))))

;;; TODO(4/18/24): optional sort by frecency/recent
(defun nvp-jump--z-directories (&optional frecency type directory)
  "Read directories from z.sh database with score of at least FRECENCY."
  (let ((data (or (getenv "_Z_DATA") (expand-file-name "~/.cache/.z")))
        (bin (expand-file-name "bin/frecent.awk" nvp/nvp)))
    (when (file-exists-p data)
      (process-lines
       shell-file-name
       shell-command-switch
       (concat
        (format "gawk -F'|' -v frecency=\"%s\" -v typ=\"%s\" -v q=\"%s\""
                (or frecency 1000)
                (or type "frecency")
                (or directory ""))
        " -f " bin " " data)))))

;;;###autoload
(defun nvp-jump-to-frecent-directory (&optional action in-dir interactive)
  "Dired in a frecent directory from z.sh database.

When IN-DIR is a file, restrict choices to its frecent sub-directories.
Otherwise, with extra prefix, or when IN-DIR is non-nil restrict to
directories under current directory."
  (interactive (list current-prefix-arg nil t))
  (and in-dir (not (stringp in-dir))
       (setq in-dir default-directory))
  (nvp-with-display-actions action
    (and interactive current-prefix-arg
         (setq in-dir default-directory))
    (when-let* ((dir (completing-read
                      (format "Frecent Dir%s:"
                              (if in-dir (concat "(" (nvp:fn in-dir) ")") ""))
                      (mapcar #'abbreviate-file-name
                              (nvp-jump--z-directories nil nil in-dir)))))
      (funcall (cond (other-window 'dired-other-window)
                     (other-frame 'dired-other-frame)
                     (t 'dired))
               (file-name-as-directory dir)))))

;;;###autoload
(defun nvp-jump-to-source (&optional action dir)
  "Jump to source file."
  (interactive "P")
  (nvp-with-display-actions action :action-order '(other same frame)
    (unless dir
      (setq dir (cond ((bound-and-true-p nvp-local-src-directories)
                       (if (length> nvp-local-src-directories 1)
                           (nvp-completing-read "Source directory: "
                             nvp-local-src-directories
                             nil t nil 'nvp-read-config-history)
                         nvp-local-src-directories))
                      (t (file-name-as-directory (getenv "DEVEL"))))))
    (let ((file (nvp-read-relative-recursively dir)))
      (if file (pop-to-buffer (find-file-noselect file))
        (user-error "Missing file")))))

;;;###autoload
(defun nvp-jump-to-org (&optional action org-file)
  "Jump to org file.
Use `nvp-local-notes-file' with an extra prefix.
Otherwise prompt, with default `nvp-default-org-file'."
  (interactive "P")
  (nvp-with-display-actions action :action-order '(other same frame)
    :other-action '((nvp-display-buffer-in-left-side-window)
                    (dedicated . t))
    (let* ((org-file (nvp-read--org-file
                      nil org-file (not current-prefix-arg)))
           (open-p (get-file-buffer org-file)))
      (or org-file (ignore-errors "Missing org file"))
      (pop-to-buffer (find-file-noselect org-file))
      (unless open-p
        (condition-case nil
            (progn (goto-char (point-min))
                   (search-forward "* Notes"))
          (error (goto-char (point-min))))))))

;;;###autoload
(defun nvp-jump-to-info (&optional action file)
  "Jump to info file (in org mode).
With prefix jump this window, otherwise `find-file-other-window'."
  (interactive (list current-prefix-arg (nvp-read--info-files)))
  (nvp-with-display-actions action
    (let ((buf (find-file-noselect file)))
      (pop-to-buffer buf)
      (unless (file-exists-p buffer-file-name)
        (nvp-display-init-template 'texinfo 'org-mode)))))

;;;###autoload
(defun nvp-jump-to-template (action)
  "Jump to a project template."
  (interactive "P")
  (nvp-with-display-actions action
    (pop-to-buffer
     (find-file-noselect (nvp-read-relative-recursively nvp/template)))))

;;;###autoload
(defun nvp-jump-to-nvp-keymap (action keymap)
  "Jump to a KEYMAP definition."
  (interactive (list current-prefix-arg (nvp-read-nvp-keymap)))
  (and (symbolp keymap)
       (setq keymap (symbol-name keymap)))
  (let ((buf (find-file-noselect
              (expand-file-name "base/nvp-bindings.el" nvp/build))))
    (with-current-buffer buf
      (goto-char (point-min))           ; might already be open
      (condition-case nil
          (when (re-search-forward
                 (concat "^(nvp[:]bindings[ ]+" (regexp-quote keymap)))
            (set-marker (mark-marker) (match-end 0)))
        (error (goto-char (point-min)))))
    (nvp-with-display-actions action
      (pop-to-buffer buf))))

;;;###autoload
(defun nvp-jump-to-register (action)
  (interactive "P")
  (when-let* ((reg (register-read-with-preview "Jump to register: ")))
    (nvp-with-display-actions action
      :same-action (nvp-display-buffer-action
                    (unless current-prefix-arg 'reuse-window)
                    'same-window)
      (funcall #'jump-to-register reg current-prefix-arg))))

;;;###autoload
(defun nvp-recentf (action)
  (interactive "P")
  (nvp-with-display-actions action
    :action-order '(same other frame)
    (consult-recent-file)))

(provide 'nvp-jump)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-jump.el ends here
