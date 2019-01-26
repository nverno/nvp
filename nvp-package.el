;;; nvp-package.el --- Manage package compile/autloads -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Maintainer: Noah Peart <noah.v.peart@gmail.com>
;; Last modified: <2019-01-25 21:05:19>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 29 November 2016

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;; Manage autoloads and compilation of site-lisp/modes etc. Follows
;; package.el conventions, most of the code is based off package.el
;;; Code:
(eval-when-compile
  (require 'autoload)
  (require 'nvp-macro)
  (nvp-local-vars))
(require 'package)

(defvar generated-autoload-file)
(defvar version-control)
(defvar autoload-timestamps)
(defvar warning-minimum-level)

;; Update the main loaddefs files from directories with autoloads
;; as well as the subdirs that need autoloads and compilation.
;;;###autoload
(defun nvp-update-all-autoloads (&optional arg force)
  (interactive)
  (cl-loop for (defs . dirs) in `((,nvp/auto ,nvp/defs ,nvp/mode ,nvp/modedefs)
                                  (,nvp/auto-site ,nvp/site))
     for generated-autoload-file = defs
     do
       (package-autoload-ensure-default-file defs)
       (mapc #'update-directory-autoloads dirs)
       (let ((buf (find-buffer-visiting defs)))
         (when buf (kill-buffer buf))))

  ;; FIXME: don't use anything in nvp/modedefs
  ;; These don't get added to load-path, but instead need to be
  ;; required when a specific mode is loaded
  (dolist (dir (directory-files nvp/modedefs t))
     (when (file-directory-p dir)
       (unless (member (file-name-nondirectory dir) '("." ".."))
         (package-generate-autoloads (file-name-nondirectory dir) dir)
         ;; add to load-path for compilation
         (add-to-list 'load-path dir)
         (nvp-package-subdir-compile dir arg force))))

  ;; Compile this directory as well
  (nvp-package-subdir-compile nvp/modedefs arg force))

;; Update autoloads and compile dir.
;;;###autoload
(defun nvp-package-update-dir (name pkg-dir &optional arg force)
  (package-generate-autoloads name pkg-dir)
  (add-to-list 'load-path pkg-dir)
  (nvp-package-subdir-compile pkg-dir arg force))

;;;###autoload
(defun nvp-package-directory-dwim (dir)
  "Guess the autoload target and whether to compile. Compile notation
R=recompile, F=force, P=if prefix.

1. `nvp/modes'(R), `nvp/defs'(F), `nvp/modedefs'(R) -> nvp/auto
2. `site-lisp'/*/*' (F)                             -> nvp/auto-site
3. './*[autoloads?|loaddefs].el' (P)                -> first match
5. default (P)                                      -> prompt"
  (interactive (list (or (bound-and-true-p dir)
                         (read-directory-name "Directory: "))))
  (let* ((generated-autoload-file
          (or (and (member dir `(,nvp/modedefs
                                 ,nvp/mode 
                                 ,nvp/defs))
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
          (and (or (string= dir nvp/defs)
                   (string= generated-autoload-file nvp/auto-site)
                   current-prefix-arg)
               0)))

    (pcase generated-autoload-file
      (`"default" (nvp-package-update-dir
                  (read-from-minibuffer "Autoloads name: ") dir nil
                  current-prefix-arg))
      (_ (progn
           (update-directory-autoloads dir)
           (nvp-package-subdir-compile dir do-compile nil))))))

;; Byte compile PKG-DIR and its subdirectories.  Just a wrapper around
;; `byte-recompile-directory'.  If ARG is 0, compile all '.el' files,
;; else if it is non-nil query the user.
;; If FORCE, recompile all '.elc' files regardless.
(defun nvp-package-subdir-compile (pkg-dir &optional arg force)
  (let ((warning-minimum-level :error)
        (save-silently inhibit-message)
        (load-path load-path))
    (byte-recompile-directory pkg-dir arg force)))

;; -------------------------------------------------------------------
;;; Recompile library

;;;###autoload
(defun nvp-package-recompile (lib)
  "Force compile files in package directory."
  (interactive (list (nvp-read "Recompile library: " :library)))
  (let ((default-directory
          (file-name-directory (locate-file lib load-path (get-load-suffixes)))))
    (byte-recompile-directory default-directory 0 t)))

(provide 'nvp-package)
;;; nvp-package.el ends here
