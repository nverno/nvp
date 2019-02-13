;;; nvp.el --- base configs -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-02-13 00:40:42>
;; Package-Requires: 
;; Created:  2 November 2016
;; Version: 1.0.0

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

;; [![Build Status](https://travis-ci.org/nverno/nvp.svg?branch=master)](https://travis-ci.org/nverno/nvp)

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(declare-function company-grab-symbol "company")

;;; TODO:
;; - separate setup from variables
;; - only search in setup when not loaded
;;   `load-history-regexp', `load-history-filename-element'
;; - use mode-local ?, better way to set many mode-local variables

;;; Aliases
(defalias 'nvp-completing-read 'ido-completing-read)
(defalias 'nvp-grab-symbol 'company-grab-symbol)
(defalias 'nvp-move-previous-defun 'beginning-of-defun)

;; root directory
(nvp-package-define-root)

;; -------------------------------------------------------------------
;;; Variables

;; movement
(defvar-local nvp-move-header-regex nil "Regex to move b/w headers.")

;; Abbrevs
(defvar-local nvp-abbrev-local-file nil "File containing local abbrev tables.")
(defvar-local nvp-abbrev-local-table nil "Abbrev table to use for mode.")
(defvar-local nvp-abbrev-dynamic-table nil "On-the-fly abbrev table.")
(defvar nvp-abbrev-prefix-chars ":<>=/#.[:alnum:]"
  "Chars to include in abbrev prefixes")

;; Snippets
(defvar-local nvp-snippet-dir nil "Directory to load for mode's snippets.")

;; functions
;; (defvar-local nvp-help-at-point-function #'nvp-help-at-point)
;; (defvar-local nvp-repl-switch-function #'nvp-repl-switch)
;; (defvar-local nvp-check-buffer-function #'nvp-validate-buffer)
(defvar-local nvp-disassemble-function #'disassemble)

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

;; -------------------------------------------------------------------
;;; general helpers

;; strip ctrl-m, multiple newlines
(defun nvp-process-buffer-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert (replace-regexp-in-string "[\r\n]+" "\n" string))))

(provide 'nvp)
;;; nvp.el ends here
