;;; nvp-pkg.el --- package compile/autoloads -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; XXX: how to expand arbitrary macros in autoloaded forms?
;;  XXX: ignore compile/autoload when missing deps
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'package)
;; (require 'autoload)
(nvp:decls)

;;; Package menu

;;;###autoload
(defun nvp-pkg-get-upgradeable (&optional fetch)
  "List packages that can be upgraded, don't update archives unless FETCH."
  (interactive "P")
  (save-window-excursion
    (if fetch (package-list-packages)
      (package-show-package-list))
    (-some->> (package-menu--find-upgrades)
      (--map (car it)))))

;;;###autoload
(defun nvp-pkg-menu-browse-url ()
  "Just jump to the url of package at point straigt off."
  (interactive)
  (--if-let (-some--> (tabulated-list-get-id)
              (cdr (assq :url (package-desc-extras it))))
      (browse-url it)
    (user-error "No URL associated with %s"
                (car (elt (tabulated-list-get-entry) 0)))))

;; -------------------------------------------------------------------
;;; Compilation

(defvar warning-minimum-level)

;; Byte compile PKG-DIR and its subdirectories.  Just a wrapper around
;; `byte-recompile-directory'.  If ARG is 0, compile all '.el' files,
;; else if it is non-nil query the user.
;; If FORCE, recompile all '.elc' files regardless.
(defun nvp-pkg-subdir-compile (pkg-dir &optional arg force)
  (let ((warning-minimum-level :error)
        (save-silently inhibit-message)
        (load-path load-path))
    (byte-recompile-directory pkg-dir arg force)))

;;;###autoload
(defun nvp-pkg-recompile (lib)
  "Force compile files in LIB directory."
  (interactive (list (call-interactively 'locate-library)))
  (let ((default-directory (file-name-directory lib)))
    (byte-recompile-directory default-directory 0 t)))

;; -------------------------------------------------------------------
;;; Autoloads

;; Update the main loaddefs files from directories with autoloads
;; as well as the subdirs that need autoloads and compilation.
;;;###autoload
(defun nvp-pkg-update-all-autoloads (&optional arg force)
  "Update loaddefs for configs and extensions.
With prefix arguments:

  \\[universal-argument]       Recompile all extension files.
  \\[universal-argument] \\[universal-argument]   FORCE compile all .el files\
 that have associated .elc files."
  (interactive "P")
  (cl-loop for (defs . dirs) in
           `(,(cons nvp/auto (list nvp/config nvp/lisp/src))
             ,(cons nvp/auto-site
                    (cons
                     nvp/thirdparty
                     (cl-remove-if-not
                      #'file-directory-p
                      (append (directory-files nvp/site t "^[^.]")
                              (directory-files nvp/pkgs t "^[^.]")
                              (directory-files nvp/modes t "^[^.]"))))))
           for autoload-file = defs
           do
           ;; (package-autoload-ensure-default-file defs)
           (loaddefs-generate dirs autoload-file nil nil nil 'generate-full)
           ;; (mapc
           ;;  (lambda (dir) (loaddefs-generate
           ;;            (file-name-as-directory dir) autoload-file))
           ;;  dirs)
           (let ((buf (find-buffer-visiting defs)))
             (when buf (kill-buffer buf)))
           when arg                           ;byte-compile as well
           do (dolist (dir dirs)
                ;; compile all .el files in site-lisp with prefix
                (nvp-pkg-subdir-compile dir (and (string= dir nvp/auto-site) arg 0)
                                        (or force (equal arg '(16)))))))

;;;###autoload
(defun nvp-pkg-update-dir (name pkg-dir &optional arg force)
  "Update directory PKG-DIR to autoloads NAME file and compile.
ARG and FORCE are passed to `byte-recompile-directory'."
  (loaddefs-generate pkg-dir name)
  (add-to-list 'load-path pkg-dir)
  (nvp-pkg-subdir-compile pkg-dir arg force))

;;;###autoload
(defun nvp-pkg-directory-dwim (dir &optional compile-all recompile-all prompt)
  "Guess the autoload target and whether to compile.

Default compile behaviour: (R=recompile; F=force recompile; P=prompt)
1. `nvp/config'(R),                  -> nvp/auto
2. `site-lisp'/*/*' (F)              -> nvp/auto-site
3. './*[autoloads?|loaddefs].el' (P) -> first match
5. default (P)                       -> prompt

When with prefix 0 or \\='-, or COMPILE-ALL, compile all \".el\" files.
With prefix [+-]4, or if RECOMPILE-ALL, recompile all \".el\" files that have
associated \".elc\".
With prefix \\='-, <0, or >=16, prompt before compiling files.
Otherwise, compile only \".el\" files with out-of-date \".elc\"."
  (interactive (let* ((raw (prefix-numeric-value current-prefix-arg))
                      (arg (abs raw)))
                 (list (read-directory-name "Directory: ")
                       (= arg 0)
                       (= arg 4)
                       (or (eq '- current-prefix-arg)
                           (< raw 0)
                           (>= arg 16)))))
  (setq dir (expand-file-name dir))
  (let* ((autoload-file
          (cond ((--some (string-prefix-p it dir)
                         (list nvp/config nvp/lisp/src))
                 nvp/auto)
                ((--some (string-prefix-p it dir)
                         (list nvp/site nvp/thirdparty))
                 nvp/auto-site)
                (t (or (car-safe (directory-files dir t "autoloads?.el"))
                       (car-safe (directory-files dir t "loaddefs?.el"))
                       'none))))
         (compile-all (and (or compile-all
                               (equal autoload-file nvp/auto-site))
                           0))
         (arg (or compile-all prompt)))
    (pcase autoload-file
      (`none (nvp-pkg-update-dir
              (read-from-minibuffer "Autoloads name: ")
              dir arg recompile-all))
      (_ (loaddefs-generate dir autoload-file)
         (nvp-pkg-subdir-compile dir arg recompile-all)))))

(provide 'nvp-pkg)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-pkg.el ends here
