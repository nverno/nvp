;;; nvp-minibuffer.el --- minibuffer helpers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-14 18:53:38>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created:  9 February 2019

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

;; minibuffer config during elisp eval / edebugging

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))

;; -------------------------------------------------------------------
;;; General to eval / edebugging
(defun nvp-minibuffer-general)

;; `(symbol-value bfn
;;; TODO:
;; - hippie-exp
;; - setup hook as elisp

;; -------------------------------------------------------------------
;;; Minibuffer elisp eval

;;; TODO: expansion from history
;; use elisp abbrevs/hippie during minibuffer evaluation
;;;###autoload
(defun nvp-minibuffer-eval-hook ()
  (setq-local )
  (setq-local hippie-expand-try-functions-list
              '(
                nvp-he-try-expand-local-abbrevs
                try-complete-lisp-symbol-partially
                try-complete-lisp-symbol))
  (setq local-abbrev-table emacs-lisp-mode-abbrev-table)
  (smartparens-mode)
  (paredit-mode)
  (eldoc-mode))

;;;###autoload
(add-hook 'eval-expression-minibuffer-setup-hook #'nvp-minibuffer-eval-hook)

(provide 'nvp-minibuffer)
;;; nvp-minibuffer.el ends here
