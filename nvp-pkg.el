;;; nvp-pkg.el --- Manage package compile/autloads -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Last modified: <2019-03-15 23:54:23>
;; URL: https://github.com/nverno/nvp
;; Created: 29 November 2016

;;; Commentary:

;;; TODO:
;; Archive:
;; - create pkg recipes and populate cache
;; Refactor regular package installs:
;; - parallel pkg install
;;   https://github.com/tttuuu888/.emacs.d/blob/master/install.el
;; Autoloads / compile
;; - generate individually instead of current single
;; - better compile logging
;; - refactor build scripts
;; Load-paths
;; - add installed pkg load paths
;; Install
;; - write install interface
;; - external install targets
;; - non-standard recipe installs
;; - external target show help / available targets
;; Display package info / dependencies
;; Interface:
;; - will need to refactor update-all-autoloads
;; - how to change current install-on-demand?
;; Uninstall
;; - support uninstalling pkgs

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro)
  (require 'autoload)
  (nvp-local-vars))
(require 'nvp)
(require 'package)

(defvar nvp-pkg-directory nvp/modes)

(cl-defstruct (nvp-pkg (:constructor nvp-pkg-make)
                       (:copier nil))
  "Personal package description.
Slots:

`name' Symbolic name of package.

`nvp-deps' List of `nvp-pkg' dependencies.

`reqs' Requirements - list of dependencies.

`dir' Directory containing package.

`ext' External dependency targets.

`extras' Optional alist of additional keyword-value pairs."
  name reqs dir ext extras)

(defun nvp-pkg--keywords (nvp-pkg)
  (let ((keywords (cdr (assoc :keywords (nvp-pkg-extras nvp-pkg)))))
    (if (eq (car-safe keywords) 'quote)
        (nth 1 keywords)
      keywords)))


;; -------------------------------------------------------------------
;;; Activation: autoloads and load-path 
;; same as package.el but don't care about versions, etc.
(defvar nvp-pkg-alist nil
  "Alist of packages available for activation.")

(defvar nvp-pkg-activated-list nil
  "List of names of currently activated packages.")

(defun nvp-pkg--autoloads-file-name (nvp-pkg)
  "Absolute name of autoloads file, sans extension."
  (expand-file-name
   (format "%s-autoloads" (nvp-pkg-name nvp-pkg))
   (nvp-pkg-dir nvp-pkg)))

(defun nvp-pkg--activate-autoloads-and-load-path (nvp-pkg)
  "Load autoloads and add package dir to `load-path'."
  (let* ((old-lp load-path)
         (pkg-dir (nvp-pkg-dir nvp-pkg))
         (pkg-dir-dir (file-name-as-directory pkg-dir)))
    (with-demoted-errors "Error loading autooloads: %s"
      (load (nvp-pkg--autoloads-file-name nvp-pkg) nil t))
    (when (and (eq old-lp load-path)
               (not (or (member pkg-dir load-path)
                        (member pkg-dir-dir load-path))))
      (push pkg-dir load-path))))

(defun nvp-pkg--load-files-for-activation (nvp-pkg reload)
  "Load files for activating package.
Load autoloads and ensure `load-path' is setup.  When RELOAD is non-nil
load all files in package corresponding to previously loaded files."
  (let* ((loaded-files-list (when reload
                              ;; can use package.el function here
                              (package--list-loaded-files (nvp-pkg-dir nvp-pkg)))))
    ;; add to load-path, add autoloads, activate package
    (nvp-pkg--activate-autoloads-and-load-path nvp-pkg)
    ;; see #<marker at 28115 in package.el> for file loading explanation
    (with-demoted-errors "Error in nvp-pkg--load-files-for-activation: %S"
      (mapc (lambda (feature) (load feature nil t))
            ;; already loaded autoloads file
            (remove (file-truename (nvp-pkg--autoloads-file-name nvp-pkg))
                    loaded-files-list)))))

(defun nvp-pkg-activate (nvp-pkg &optional reload)
  "Activate package, even if already active.
If RELOAD is non-nil, also `load' any files inside the package which 
correspond to previously loaded files (`package--list-loaded-files')."
  (let* ((name (nvp-pkg-name nvp-pkg))
         (pkg-dir (nvp-pkg-dir nvp-pkg)))
    (unless pkg-dir
      (error "Internal error: unable to find directory for `%s'"
             (nvp-pkg-name nvp-pkg)))
    (nvp-pkg--load-files-for-activation nvp-pkg reload)
    (push name nvp-pkg-activated-list)
    t))


;; -------------------------------------------------------------------
;;; Autoloads
;; package.el:
;; - `package-autoload-ensure-default-file'
;; - `package-generate-autoloads'

;; (require 'autoload)
(defvar version-control)
(defvar autoload-timestamps)


;; -------------------------------------------------------------------
;;; Compilation
(defvar warning-minimum-level)

;; Byte compile PKG-DIR and its subdirectories.  Just a wrapper around
;; `byte-recompile-directory'.  If ARG is 0, compile all '.el' files,
;; else if it is non-nil query the user.
;; If FORCE, recompile all '.elc' files regardless.
(defun nvp-package-subdir-compile (pkg-dir &optional arg force)
  (let ((warning-minimum-level :error)
        (save-silently inhibit-message)
        (load-path load-path))
    (byte-recompile-directory pkg-dir arg force)))

;;;###autoload
(defun nvp-package-recompile (lib)
  "Force compile files in LIB directory."
  (interactive (list (call-interactively 'locate-library)))
  (let ((default-directory (file-name-directory lib)))
    (byte-recompile-directory default-directory 0 t)))

(defun nvp-pkg--compile (nvp-pkg)
  "Byte-compile package.
This assumes package has been activated."
  (let ((warning-minimum-level :error)
        (save-silently inhibit-message)
        (load-path load-path))
    (byte-recompile-directory (nvp-pkg-dir nvp-pkg) 0 t)))


;; -------------------------------------------------------------------
;;; Archives
;; list available packages

(defvar nvp-pkg-archive-contents nil
  "Store list of available packages.
An alist mapping package names to `nvp-pkg' structures.")

(defun nvp-pkg-refresh-contents ()
  "Read archive contents."
  (let ((filename (expand-file-name "archive-contents" nvp-pkg-directory)))
    (when (file-exists-p filename)
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8))
          (insert-file-contents filename))
        (setq nvp-pkg-archive-contents (cdr (read (current-buffer))))))))


;; -------------------------------------------------------------------
;;; Initialize

(defvar nvp-pkg--initialized nil)

(defun nvp-pkg-initialize (&optional no-activate)
  "Load packages and activate them unless NO-ACTIVATE is non-nil."
  (interactive)
  (when (and nvp-pkg--initialized (not after-init-time))
    (lwarn '(package reinitialization) :warning
           "Unnecessary call to `nvp-pkg-initialize' in init file"))
  (setq nvp-pkg-alist (nvp-pkg-refresh-contents))
  (setq nvp-pkg--initialized t)
  (unless no-activate
    (nvp-pkg-activate-all)))

(defun nvp-pkg-activate-all ()
  "Activate all installed packages."
  (unless nvp-pkg--initialized
    (nvp-pkg-initialize t))
  (dolist (elt nvp-pkg-alist)
    (condition-case err
        (nvp-pkg-activate (car elt))
      (error (message "%s" (error-message-string err))))))

;; -------------------------------------------------------------------
;;; Description
;; TODO: pretty print pkg details
;; ;; `package--get-deps' to get package installed info
;; (defun nvp-pkg--get-deps (pkg )
;;   )


;; -------------------------------------------------------------------
;;; Install

(defun nvp-pkg-installed-p (pkg)
  "Return non-nil if PKG is installed.
PKG can either be a `nvp-pkg' or symbol."
  (cond
   ((nvp-pkg-p pkg)
    (memq (nvp-pkg-name pkg) nvp-pkg-alist))
   (t (memq pkg nvp-pkg-alist))))

;;; TODO: #<marker at 84915 in package.el>
;;;###autoload
(defun nvp-pkg-install (pkg)
  "Install PKG.
PKG can be a `nvp-pkg' or a symbol naming an available package in 
`nvp-pkg-archives'."
  (interactive
   (progn
     ;; Initialize to get list of package symbols for completion
     (unless nvp-pkg--initialized
       (nvp-pkg-initialize t))
     (unless nvp-pkg-archive-contents
       (nvp-pkg-refresh-contents))
     (list (intern (completing-read
                    "Install nvp pkg: "
                    (delq nil
                          (mapcar (lambda (elt)
                                    (unless (nvp-pkg-installed-p (car elt))
                                      (symbol-name (car elt))))
                                  nvp-pkg-archive-contents))
                    nil t)))))
  pkg)


;; -------------------------------------------------------------------
;;; Old commands

;; Update the main loaddefs files from directories with autoloads
;; as well as the subdirs that need autoloads and compilation.
;;;###autoload
(defun nvp-update-all-autoloads (&optional arg force)
  "Update loaddefs for configs and extensions.
With prefix ARG recompile all extension files. 
With double prefix, FORCE compile all .el files with associated .elc file."
  (interactive "P")
  (cl-loop for (defs . dirs) in `(,(cons nvp/auto (list nvp/config))
                                  ,(cons nvp/auto-site (list nvp/site)))
     for generated-autoload-file = defs
     do
       (package-autoload-ensure-default-file defs)
       (mapc #'update-directory-autoloads dirs)
       (let ((buf (find-buffer-visiting defs)))
         (when buf (kill-buffer buf)))
     when arg                           ;byte-compile as well
     do (dolist (dir dirs)
          ;; compile all .el files in site-lisp with prefix
          (nvp-package-subdir-compile dir (and (string= dir nvp/auto-site) arg 0)
                                      (or force (equal arg '(16)))))))

;;;###autoload
(defun nvp-package-update-dir (name pkg-dir &optional arg force)
  "Update directory PKG-DIR to autoloads NAME file and compile.
ARG and FORCE are passed to `byte-recompile-directory'."
  (package-generate-autoloads name pkg-dir)
  (add-to-list 'load-path pkg-dir)
  (nvp-package-subdir-compile pkg-dir arg force))

;;;###autoload
(defun nvp-package-directory-dwim (dir)
  "Guess the autoload target and whether to compile. Compile notation
R=recompile, F=force, P=if prefix.

1. `nvp/config'(R),  -> nvp/auto
2. `site-lisp'/*/*' (F)                             -> nvp/auto-site
3. './*[autoloads?|loaddefs].el' (P)                -> first match
5. default (P)                                      -> prompt"
  (interactive (list (read-directory-name "Directory: ")))
  (let* ((generated-autoload-file
          (or (and (member dir `(,nvp/config))
                   nvp/auto)
              (and (member nvp/site
                           `(,(file-name-directory
                               (directory-file-name dir))
                             ,dir))
                   nvp/auto-site)
              (car-safe (directory-files dir t "autoloads?.el"))
              (car-safe (directory-files dir t "loaddefs?.el"))
              "default"))
         (do-compile
          (and (or (string= generated-autoload-file nvp/auto-site)
                   current-prefix-arg)
               0)))

    (pcase generated-autoload-file
      (`"default" (nvp-package-update-dir
                  (read-from-minibuffer "Autoloads name: ") dir nil
                  current-prefix-arg))
      (_ (progn
           (update-directory-autoloads dir)
           (nvp-package-subdir-compile dir do-compile nil))))))

(provide 'nvp-pkg)
;;; nvp-pkg.el ends here
