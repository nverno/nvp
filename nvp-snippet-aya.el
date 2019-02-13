;;; nvp-snippet-aya.el --- auto-snippets -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-13 04:45:07>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 12 February 2019

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
;; auto yasnippets
;;; Code:
(eval-when-compile
  (defvar aya-current))
(require 'auto-yasnippet)
(nvp-declare "" nvp-jump-to-new-snippet)

(defvar nvp-aya-new-template "\
# -*- mode: snippet -*-
# name: $1
# key: ${2:${1:$(yas--key-from-desc yas-text)}}
# --
`aya-current`")

;;;###autoload(autoload 'nvp-aya-hydra/body "nvp-snippet-aya")
(nvp-hydra-set-property 'nvp-aya-hydra)
(defhydra nvp-aya-hydra (:color blue)
  ("c" aya-create "create(~)")
  ("e" aya-expand "expand")
  ("j" nvp-jump-to-aya-snippet "jump/persist")
  ("o" aya-create-one-line "one-liner($)"))

;;;###autoload
(defun nvp-jump-to-aya-snippet ()
  "Save previously created auto-yasnippet."
  (interactive
   (if (string-empty-p aya-current)
       (user-error "No current auto-snippet.")
     (list aya-current)))
  (nvp-jump-to-new-snippet major-mode nvp-snippet-dir nil aya-current
                           nvp-aya-new-template))

(provide 'nvp-snippet-aya)
;;; nvp-snippet-aya.el ends here
