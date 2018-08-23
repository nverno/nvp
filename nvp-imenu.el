;;; nvp-imenu ---   -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 25 January 2017

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
(require 'idomenu nil t)
(require 'nvp-yas)

;;; Dynamic variables

;; top-level header regexp
(defvar-local nvp-imenu-comment-headers-re nil)

;; header regexp nested under "Headers"
(defvar-local nvp-imenu-comment-headers-re-1 nil)

;; sub-headers
(defvar-local nvp-imenu-comment-headers-re-2 nil)

;; -------------------------------------------------------------------
;;; Hook

;; make header from comment
;;;###autoload
(cl-defun nvp-imenu-setup (&key headers headers-1 headers-2)
  (setq nvp-imenu-comment-headers-re
        (or headers 
            `((nil ,(concat "^" (regexp-quote (nvp-yas-comment 3))
                            "\\s-*\\(.*\\)\\s-*$")
                   1))))
  (setq nvp-imenu-comment-headers-re-1
        (or headers-1 `(("Headers" ,(cadr (car nvp-imenu-comment-headers-re))
                         1))))
  (setq nvp-imenu-comment-headers-re-2
        (or headers-2
            `(("Sub-Headers"
               ,(concat "^" (nvp-yas-comment 2) "-+\\s-*\\(.*\\)[ -]*$")
               1))))
  (setq-local imenu-generic-expression
              (append nvp-imenu-comment-headers-re-1 imenu-generic-expression)))

;; -------------------------------------------------------------------
;;; Util

(eval-when-compile
  (defmacro ido/imenu ()
    (if (featurep 'idomenu) '(idomenu) '(imenu))))

;; -------------------------------------------------------------------
;;; Commands

;;;###autoload
(defun nvp-imenu-idomenu (arg)
  (interactive "P")
  (pcase arg
    (`(4)
     ;; headers only
     (let ((imenu-generic-expression nvp-imenu-comment-headers-re)
           (imenu-create-index-function 'imenu-default-create-index-function))
       (condition-case nil
           (ido/imenu)
         (error (message "nvp-imenu-comment-headers-re: %s"
                         nvp-imenu-comment-headers-re)))))
    (`(16)
     ;; headers + sub-headers
     (let ((imenu-generic-expression
            (append nvp-imenu-comment-headers-re-1
                    nvp-imenu-comment-headers-re-2))
           (imenu-create-index-function 'imenu-default-create-index-function))
       (ido/imenu)))
    (_ (ido/imenu))))

;; -------------------------------------------------------------------

(declare-function idomenu "idomenu")

(provide 'nvp-imenu)
;;; nvp-imenu.el ends here
