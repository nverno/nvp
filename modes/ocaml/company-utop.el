;;; company-utop ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/ocaml-tools
;; Package-Requires: 
;; Created:  9 December 2016

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
(require 'utop-inf)
(require 'company)

(defun company-utop--candidates (arg)
  (setq utop-inf-completions nil)
  (utop-inf-redirect arg)
  utop-inf-completions)

(defun company-utop--prefix ()
  (and (eq major-mode 'utop-mode)
       (buffer-substring utop-prompt-max (point))))

;; (defun company-utop--annotation (candidate)
;;   (or (get-text-property 0 'annot candidate) ""))

;; (defun company-utop--meta (candidate)
;;   (get-text-property 0 'meta candidate))

;; (defun company-utop--doc (candidate)
;;   (company-doc-buffer ""))

;; (defun company-utop--location (candidate))

;;;###autoload
(defun company-utop (command &optional arg &rest _args)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-utop))
    (prefix (company-utop--prefix))
    ;; (annotation (company-utop--annotation arg))
    ;; (doc-buffer (company-utop--doc arg))
    ;; (location (company-utop--location arg))
    (candidates (company-utop--candidates arg))
    (require-match 'never)
    (duplicates nil)
    (sorted t)))

(provide 'company-utop)
;;; company-utop.el ends here
