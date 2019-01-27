;;; nvp-hook.el --- hooks -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-01-27 01:29:21>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Maintainer: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 14 January 2019

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
  (require 'nvp-macro))
(require 'time-stamp)

;;;###autoload
(defun nvp-hook-update-timestamp ()
  "Update buffer time stamps - `before-save-hook'."
  (let ((time-stamp-pattern 
         (or time-stamp-pattern
             (pcase major-mode
               (`org-mode "#\\+DATE: <%%>$")
               (_ "15/Last modified: <%%>$")))))
    (time-stamp)))

;; -------------------------------------------------------------------
;;; Remove lsp stuff

(eval-when-compile
  (defvar eglot--saved-bindings))
(declare-function flymake-mode "flymake")
(declare-function eglot-completion-at-point "eglot")
(declare-function eglot--eldoc-message "eglot")
(declare-function eglot-imenu "eglot")
(declare-function eglot-xref-backend "eglot")

;;;###autoload
(cl-defun nvp-hook-eglot-shutup (&key (completion t) (eldoc t) (flymake t) (imenu t)
                                      xref)
  "Remove eglot hooks/advices that clobber stuff.
By default remove all but XREF."
  (and completion
       (remove-hook 'completion-at-point-functions 'eglot-completion-at-point t))
  (and flymake (flymake-mode -1))
  (and xref (remove-hook 'xref-backend-functions 'eglot-xref-backend t))
  (when eldoc
    (remove-function (local 'eldoc-message-function) #'eglot--eldoc-message)
    (and (bound-and-true-p eglot--saved-bindings)
         (eval
           `(nvp-eldoc-function
             ,(cdr (assoc 'eldoc-documentation-function eglot--saved-bindings))
             'no-init))))
  (and imenu (remove-function (local 'imenu-create-index-function) #'eglot-imenu)))

(provide 'nvp-hook)
;;; nvp-hook.el ends here
