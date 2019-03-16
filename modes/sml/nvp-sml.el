;;; nvp-sml.el --- smluts -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp-sml
;; Last modified: <2019-03-16 00:02:39>
;; Package-Requires: 
;; Created:  2 November 2016

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

;; [![Build Status](https://travis-ci.org/nverno/nvp-sml.svg?branch=master)](https://travis-ci.org/nverno/nvp-sml)

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro)
  (nvp-local-vars))
(require 'sml-mode)
(declare-function tag-utils-tag-dir "tag-utils")
(declare-function smie-forward-sexp "smie")

(defvar nvp-sml-src-repo "https://smlnj-gforge.cs.uchicago.edu/svn")
(defvar nvp-sml-src-dir (expand-file-name "sml" (getenv "DEVEL")))

;; ------------------------------------------------------------
;;; Install / Tags

;; (defun nvp-sml-install ()
;;   (interactive)
;;   (nvp-with-install-script nvp-sml--dir "install_sml_deps" 'sudo
;;     (nvp-with-install-script nvp-sml--dir "install" nil
;;       (nvp-with-install-script nvp-sml--dir "install_sml_source" nil
;;         (comint-mode)))))

;; clone / update source, tag it
;; with two prefix arg, reinstall source if already have it
;; with single prefix, force retag
(defun nvp-sml-tag-source (arg &optional noretry)
  (interactive "P")
  (let ((have-src (file-exists-p nvp-sml-src-dir))
        (tags (expand-file-name "TAGS" nvp-sml-src-dir)))
    (if (and (not noretry)
             (or (not have-src) (eq '(16) arg)))
        ;; Get source / reinstall if have
        (progn
          (when have-src
            (delete-directory nvp-sml-src-dir))
          ;; (nvp-with-install-script nvp-sml--dir "install_sml_source" nil
          ;;   (nvp-sml-tag-source nil t))
          )
      ;; Otherwise, tag source / load tags table
      (when have-src
        (if (and (not (eq '(4) arg))
                 (file-exists-p tags))
            ;; without prefix arg, just load tags table
            (visit-tags-table tags)
          (tag-utils-tag-dir nvp-sml-src-dir))))))

;; ------------------------------------------------------------
;;; Interactive

(nvp-newline nvp-sml-newline-dwim
  "Newline dwim for `sml-mode'"
  :comment-re (" *\\(?:(\\*\\|\\*\\)" . "\\*) *")
  :comment-start "* ")

;; mark defuns successively when called repeatedly
(defun nvp-sml-mark-defun ()
  (interactive)
  (if (or (and (eq last-command this-command) (mark t))
          (and transient-mark-mode mark-active))
      (set-mark
       (save-excursion
         (goto-char (mark))
         (smie-forward-sexp 'halfsexp)
         (point)))
    (sml-mark-function)))

;;; Movement

(eval-when-compile
  (defmacro nvp-sml--search (regexp &optional back)
    `(let ((start (point)))
       (condition-case nil
           (progn
             (forward-line ,(if back -1 1))
             (,(if back 're-search-backward 're-search-forward) ,regexp)
             (beginning-of-line))
         (error (goto-char start))))))

(defun nvp-sml-previous-defun ()
  (interactive)
  (if (bolp)
      (nvp-sml--search "^fun" 'back)
    (sml-beginning-of-defun)))

(defun nvp-sml-next-defun ()
  (interactive)
  (if (bolp)
      (nvp-sml--search "^fun")
    (sml-beginning-of-defun)
    (smie-forward-sexp 'halfsexp)
    (nvp-sml--search "^fun")))

;; -------------------------------------------------------------------
;;; REPL

(declare-function sml-prog-proc-switch-to "sml-mode")

(defun nvp-sml-inf-newline ()
  (interactive)
  (end-of-line)
  (insert ";")
  (comint-send-input))

;; (nvp-repl-switch "sml" (:repl-mode 'inferior-sml-mode
;;                                    ))
(defvar nvp-sml--last-buffer nil)
(defun nvp-sml-switch-buffers ()
  (interactive)
  (if (and (eq major-mode 'inferior-sml-mode)
           nvp-sml--last-buffer)
      (switch-to-buffer-other-window nvp-sml--last-buffer)
    (setq nvp-sml--last-buffer (current-buffer))
    (sml-prog-proc-switch-to)))

;; manage/hippie-expand on shell history
;;;###autoload(autoload 'nvp-sml-inf-hippie "nvp-sml")
(nvp-hippie-shell-fn nvp-sml-inf-hippie ".sml_history")

;;;###autoload
(add-hook 'inferior-sml-mode-hook 'nvp-sml-inf-hippie)

(provide 'nvp-sml)
;;; nvp-sml.el ends here
