;;; nvp-disassemble.el --- disassembly -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-14 16:18:53>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires:
;; Created:  8 February 2019

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
;; Mode local functions
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'nvp-macro))
(require 'nvp)

;; -------------------------------------------------------------------
;;; Disassemble code
;; - lisp, c, java

;;;###autoload
(defun nvp-disassemble (popup)
  "Call local function `nvp-disassemble-function'."
  (interactive "P")
  (if popup
      (nvp-disassemble-doc)
   (call-interactively nvp-disassemble-function)))

(cl-defgeneric nvp-disassemble-doc ()
  "Return docstring with disassembly."
  (user-error "`nvp-disassemble-doc' not implemented for %S" major-mode))

(cl-defmethod nvp-disassemble-doc
  (&context ((not (null (derived-mode-p 'emacs-lisp-mode 'inferior-emacs-lisp-mode))) (eql t)))
  (message "BLAH"))

(cl-defmethod nvp-disassemble-doc
  (&context (major-mode comint-mode)) ()
  (user-error "TODO"))

(defun nvp-disassemble-popup ()
  (interactive)
  (let ((doc (nvp-disassemble-doc)))
    (nvp-with-toggled-tip doc)))

(provide 'nvp-disassemble)
;;; nvp-disassemble.el ends here
