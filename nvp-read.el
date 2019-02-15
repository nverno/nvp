;;; nvp-read.el --- Completing read for thangs -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-02-15 10:09:19>
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

;;; TODO:
;; - read w/ popup help: see `register-read-with-preview'

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib)
  (require 'nvp))
(require 'nvp)                          ;nvp-prompt--with-default
(declare-function function-called-at-point "help")
(declare-function help--symbol-completion-table "help-fns")
(autoload 'eldoc-minibuffer-message "eldoc")

(defsubst nvp-read--relative-files (root regexp)
  (mapcar (lambda (f) (file-relative-name f root))
          (directory-files-recursively root regexp)))

;;;###autoload
(defun nvp-read-relative-recursively (root regexp &optional prompt default)
  "Return full filepath prompting with file matching REGEXP from ROOT with\
`directory-files-recursively'."
  (expand-file-name
   (nvp-completing-read
    (nvp-prompt--with-default (or prompt "File: ") default)
    (nvp-read--relative-files root regexp) nil nil nil
    'nvp-read-config-history default)
   root))

;; some completing reads for general config files
(defun nvp-read-mode-config (&optional prompt default)
  (unless default
    (setq default (symbol-name major-mode)))
  (setq prompt (nvp-prompt--with-default (or prompt "Mode config: ") default))
  (nvp-completing-read
   prompt
   (mapcar
    #'(lambda (x) ;; ignore preceding 'nvp-' and ending '-config.el'
        (replace-regexp-in-string "\\(nvp-\\|\\(?:-config\\)?\\.el\\)" "" x))
    (directory-files nvp/config nil "^[^\\.].*\\.el$"))
   nil nil nil 'nvp-read-config-history (substring default 0 -5)))

(defun nvp-read--info-files (&optional prompt)
  (expand-file-name 
   (nvp-completing-read
    (or prompt "Info file: ")
    (directory-files (expand-file-name "org" nvp/info) nil "\.org")
    nil nil nil 'nvp-read-config-history)
   (concat nvp/info "org")))

(defun nvp-read--mode-test (&optional prompt default)
  (let* ((ext (ignore-errors (file-name-extension (buffer-file-name))))
         (default-directory nvp/test)
         (completion-ignored-extensions
          (cons "target" completion-ignored-extensions))
         (files (nvp-read--relative-files nvp/test "^[^.][^.]")))
    (unless default
      (setq default (and ext (cl-find-if (lambda (f) (string-suffix-p ext f)) files))))
    (setq prompt (nvp-prompt--with-default (or prompt "Test: ") default))
    (expand-file-name
     (nvp-completing-read
      prompt files nil nil nil 'nvp-read-config-history default)
     nvp/test)))

;; if LOCAL is non-nil use that
(defun nvp-read--org-file (&optional prompt default nolocal)
  (let ((local (bound-and-true-p nvp-local-notes-file)))
    (if (and local (not nolocal)) local
      (or default (setq default nvp-default-org-file))
      (setq prompt (nvp-prompt--with-default (or prompt "Org file: ") default))
      (nvp-read-relative-recursively
       nvp/org "\.org$" (or prompt "Org file: ") default))))

;; -------------------------------------------------------------------
;;; Minibuffer input

;;;###autoload
(defun nvp-read-with-message (prompt &optional format-string &rest args)
  "Display message in mode-line while reading from minibuffer."
  (minibuffer-with-setup-hook
      (:append (lambda () (eldoc-minibuffer-message format-string args)))
    (read-from-minibuffer prompt)))

;; -------------------------------------------------------------------
;;; Elisp objects 

;;;###autoload
(defun nvp-read-obarray-regex (prompt &optional regexp default)
  "Completing read for obarray with optional REGEXP filter."
  (completing-read prompt obarray
                   (lambda (sym) (string-match-p regexp (symbol-name sym)))
                   t nil nil
                   (and default (if (symbolp default) (symbol-name default)
                                  default))))

;; #<marker at 34938 in help-fns.el.gz>
;;;###autoload
(defun nvp-read-elisp-symbol (prompt &optional predicate default)
  "Read symbol using `help--symbol-completion-table' using PROMPT with DEFAULT.
Filter by PREDICATE if non-nil."
  (require 'help-fns)
  (let ((enable-recursive-minibuffers t) val)
    (setq prompt (nvp-prompt--with-default prompt default))
    (setq val (completing-read prompt #'help--symbol-completion-table
                               predicate t nil nil
                               (if (and default (symbolp default))
                                   (symbol-name default))))
    (unless (equal val "")
      (intern val))))

;;;###autoload
(defun nvp-read-elisp-variable (prompt &optional default)
  "Lookup elisp symbol using PROMPT and optional DEFAULT."
  (unless default (setq default (variable-at-point)))
  (let ((orig-buffer (current-buffer)))
    (nvp-read-elisp-symbol prompt
                           (lambda (vv)
                             (with-current-buffer orig-buffer
                               (or (get vv 'variable-documentation)
                                   (and (boundp vv) (not (keywordp vv)))))))))

;;;###autoload
(defun nvp-read-elisp-function (prompt &optional default)
  "Lookup elisp function with PROMPT and optional DEFAULT."
  (unless default (setq default (function-called-at-point)))
  (nvp-read-elisp-symbol
   prompt (lambda (f) (or (fboundp f) (get f 'function-documentation))) default))

;;;###autoload
(defun nvp-read-mode (&optional default)
  "Lookup name of mode using PROMPT and optional DEFAULT."
  (unless default (setq default major-mode))
  (let ((prompt (if default (format "Mode (default \"%s\"): " default)
                  "Mode: ")))
    (nvp-read-obarray-regex prompt "-mode\\'" default)))

(provide 'nvp-read)
;;; nvp-read.el ends here
