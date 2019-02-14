;;; nvp-read.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-02-13 22:53:25>
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
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'nvp)
(declare-function function-called-at-point "help")
(declare-function help--symbol-completion-table "help-fns")
(autoload 'eldoc-minibuffer-message "eldoc")

;;; TODO:
;; - read w/ popup help: see `register-read-with-preview'

;; add default to prompt
(defsubst nvp-read--with-default (prompt &optional default)
  (if default
      (format "%s (default %s): "
              (substring prompt 0 (string-match "[ :]+\\'" prompt))
              default)
    prompt))

;; minibuffer history
(defvar nvp-mode-config-history ())

;; some completing reads for general config files
(defun nvp-read--mode-config (&optional prompt default)
  (unless default
    (setq default (symbol-name major-mode)))
  (setq prompt (nvp-read--with-default (or prompt "Mode config: ") default))
  (nvp-completing-read
   prompt
   (mapcar
    #'(lambda (x) ;; ignore preceding 'nvp-' and ending '-config.el'
        (replace-regexp-in-string "\\(nvp-\\|\\(?:-config\\)?\\.el\\)" "" x))
    (directory-files nvp/config nil "^[^\\.].*\\.el$"))
   nil nil nil 'nvp-mode-config-history (substring default 0 -5)))

(defun nvp-read--info-files (&optional prompt)
  (expand-file-name 
   (nvp-completing-read
    (or prompt "Info file: ")
    (directory-files (expand-file-name "org" nvp/info) nil "\.org"))
   (concat nvp/info "org")))

(defun nvp-read--mode-test (&optional prompt default)
  (let* ((ext (ignore-errors (file-name-extension (buffer-file-name))))
         (default-directory nvp/test)
         (completion-ignored-extensions
          (cons "target" completion-ignored-extensions))
         (files (mapcar (lambda (f) (file-relative-name f nvp/test))
                        (directory-files-recursively nvp/test "^[^.][^.]")))
         (def (and ext (cl-find-if (lambda (f) (string-suffix-p ext f)) files))))
    (setq prompt (nvp-read--with-default (or prompt "Test: ") def))
    (expand-file-name
     (nvp-completing-read
      prompt files nil nil nil 'nvp-config-file-history def)
     nvp/test)))

(defun nvp-read--org-file (&optional prompt default)
  (or default (setq default "gtd.org"))
  (setq prompt (nvp-read--with-default (or prompt "Org file: ") default))
  (ido-completing-read
   prompt
   (expand-file-name (directory-files nvp/org nil "^[^.]") nvp/org) nil nil nil
   'nvp-config-file-history default))

;; -------------------------------------------------------------------

;;;###autoload
(defun nvp-read-with-message (prompt &optional format-string &rest args)
  "Display message in mode-line while reading from minibuffer."
  (minibuffer-with-setup-hook
      (:append (lambda () (eldoc-minibuffer-message format-string args)))
    (read-from-minibuffer prompt)))

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
    (setq prompt (nvp-read--with-default prompt default))
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
