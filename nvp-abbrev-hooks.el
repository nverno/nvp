;;; nvp-abbrev-hooks.el --- abbrev expansion predicates -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-09 04:28:54>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created:  6 February 2019

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
(declare-function expand-abbrev-hook "expand")

;; -------------------------------------------------------------------
;;; Post insert
;; C level only calls `expand-abbrev' when preceding char is word syntax
;; so hook into `post-self-insert-hook'
;;;###autoload
(defun nvp-abbrev-expand-after-symbols-hook ()
  (and (equal (syntax-after (1- (point))) '(3))
       (setq this-command 'nvp-abbrev-expand-after-symbols)
       (expand-abbrev)))

;; -------------------------------------------------------------------
;;; Expand Hooks

;; allow abbrevs to expand inside parens
;;;###autoload
(defun nvp-abbrev-expand-in-paren-hook ()
  (cl-letf (((symbol-function 'eolp)
             #'(lambda () (not (eq (char-syntax (char-after)) ?w)))))
    (expand-abbrev-hook)))

;; -------------------------------------------------------------------
;;; Expansion predicates

;; dont expand in strings/comments
;;;###autoload
(defun nvp-abbrev-expand-p ()
  (let ((ppss (syntax-ppss)))
    (not (or (elt ppss 3) (elt ppss 4)))))

;; don't expand in strings/comments or after [_.-:]
;;;###autoload
(defun nvp-abbrev-expand-not-after-punct-p ()
  (and (not (memq last-input-event '(?_ ?. ?- ?:)))
       (let ((ppss (syntax-ppss)))
         (not (or (elt ppss 3) (elt ppss 4))))))

;; -------------------------------------------------------------------
;;; Abbrev edit

;;;###autoload
(defun nvp-abbrev-edit-hook ()
  (setq-local imenu-generic-expression '((nil "^(\\(.*\\))" 1))))
;;;###autoload(add-hook 'edit-abbrevs-mode-hook #'nvp-abbrev-edit-hook)

(provide 'nvp-abbrev-hooks)
;;; nvp-abbrev-hooks.el ends here
