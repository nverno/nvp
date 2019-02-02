;;; nvp-bind.el --- bindings  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-02 02:32:45>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Maintainer: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created:  1 February 2019

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

;;;###autoload
(defun nvp-bind-transient-key (key cmd &optional keep exit)
  "Bind KEY to CMD in transient map."
  (set-transient-map
   (let ((tmap (make-sparse-keymap)))
     (define-key tmap (kbd key) cmd)
     tmap)
   (or keep t)
   (or exit nil)))

;;;###autoload
(defun nvp-bind-local-bindings (bindings &optional buffer)
  "Use buffer local BINDINGS.
Optionally use them in BUFFER instead of current buffer."
  (let ((lmap (make-sparse-keymap)))
    (set-keymap-parent lmap (current-local-map))
    (dolist (b bindings)
      (define-key lmap (kbd (car b)) (cadr b)))
    (if buffer
        (with-current-buffer buffer
          (use-local-map lmap))
      (use-local-map lmap))))

;; Overrides a minor mode keybinding for the local buffer by creating
;; or altering keymaps stored in buffer-local variable 
;; `minor-mode-overriding-map-alist'.
;;;###autoload
(defun nvp-bind-minor-mode-override-key (mode key def)
  "Override a minor MODE KEY DEF using `minor-mode-overriding-map-alist'."
  (let ((map (make-sparse-keymap)))
    (define-key map key def)
    (push (cons mode map) minor-mode-overriding-map-alist)))

(provide 'nvp-bind)
;;; nvp-bind.el ends here
