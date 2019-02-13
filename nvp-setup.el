;;; nvp-setup.el --- setup hooks/helpers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-13 08:13:56>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 13 February 2019

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
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'nvp-macro))
(require 'nvp)

;; -------------------------------------------------------------------
;;; Helpers

;; macro version unless needing to mapc
;; (defun nvp-setup-program (name &optional path)
;;   (and (symbolp name) (setq name (symbol-name name)))
;;   (and path (substitute-env-in-file-name path))
;;   (nvp-with-gnu/w32
;;       (cl-loop for dir in (delq (cons path nvp-program-search-paths))
;;          as f = (expand-file-name ,name dir)
;;          when (file-exists-p f)
;;          return f)
;;     (or (bound-and-true-p ()))))

;; ------------------------------------------------------------
;;; Setup

;;;###autoload
(defun nvp-setup-package-root (pkg)
  "Return a guess for the package root directory."
  (let* ((sym (and pkg (if (stringp pkg) (intern-soft pkg) pkg)))
         (str (and pkg (if (stringp pkg) pkg (symbol-name pkg))))
         (path                          ;path to package
          (cond
           ;; should be able to find features and autoloads
           ((and sym (featurep sym) (locate-library str)))
           ((and sym                    ;autoload / already loaded
                 (ignore-errors (locate-library (symbol-file sym)))))
           ;; check if pkg exists or is on load-path
           ((and (stringp str)
                 (if (file-exists-p pkg) pkg
                   ;; look for package directory
                   (locate-file str load-path nil
                                (lambda (f) (if (file-directory-p f) 'dir-ok))))))
           (t nil))))
    ;; return directory
    (if path
      (if (file-directory-p path) path
        (directory-file-name (file-name-directory path))))))

;;;###autoload
(cl-defun nvp-setup-local
    (name
     &key
     mode
     abbr-file
     snippets-dir
     (dir (nvp-setup-package-root name))
     (snippets (concat "snippets/" (or snippets-dir mode (symbol-name major-mode))))
     (abbr-table (or mode (symbol-name major-mode)))
     (fn nil))
  "Setup local variables for helper package - abbrevs, snippets, root dir."
  (if (not (file-exists-p dir))
      (user-error "Setup for '%s' failed to find package root"
                  (if (symbolp name) (symbol-value name) name))
    (or abbr-file
        (setq abbr-file (ignore-errors
                          (car (directory-files dir nil "abbrev-table")))))
    (setq-local nvp-snippet-dir (expand-file-name snippets dir))
    (setq-local nvp-abbrev-local-table abbr-table)
    (and abbr-file
         (setq-local nvp-abbrev-local-file (expand-file-name abbr-file dir))
         (ignore-errors (quietly-read-abbrev-file nvp-abbrev-local-file)))
    (setq-local local-abbrev-table
                (symbol-value
                 (intern-soft (concat nvp-abbrev-local-table "-abbrev-table")))))
  (when fn (funcall fn)))

(provide 'nvp-setup)
;;; nvp-setup.el ends here
