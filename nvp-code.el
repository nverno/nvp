;;; nvp-code.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-02-07 14:13:54>
;; Package-Requires: 
;; Created: 25 November 2016

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
(nvp-declare "hideshow" hs-toggle-hiding hs-show-all hs-hide-all)

;; -------------------------------------------------------------------
;;; Code Fold

(defvar nvp-hs--hidden nil)

(defun nvp-hs-toggle (&optional arg)
  (interactive "P")
  (unless hs-minor-mode
    (hs-minor-mode))
  (if arg
      (if (setq nvp-hs--hidden (not nvp-hs--hidden))
          (hs-show-all)
        (hs-hide-all))
    (hs-toggle-hiding)))

;;;###autoload
(defun nvp-hs-init (&optional arg)
  (interactive "P")
  (hs-minor-mode)
  (nvp-bind-keys nvp-narrow/fold-keymap
    ("f"   . nil)
    ("f f" . nvp-hs-toggle)
    ("f a" . hs-hide-all)
    ("f s" . hs-show-all))
  (nvp-hs-toggle arg))

(provide 'nvp-code)
;;; nvp-code.el ends here
