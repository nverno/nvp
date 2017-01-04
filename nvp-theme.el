;;; nvp-theme ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created:  4 January 2017

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
(require 'moe-theme)
(require 'powerline)

(defun nvp-theme-light ()
  (setq moe-theme-highlight-buffer-id t)
  (setq moe-theme-mode-line-color 'green)
  (moe-light)
  (powerline-moe-theme))

(defun nvp-theme-dark ()
  (load-theme 'gruvbox)
  (setq moe-theme-mode-line-color 'blue)
  (powerline-moe-theme))

;; toggle light/dark themes
;;;###autoload
(defun nvp-theme-switch ()
  (interactive)
  (if (not (eq last-command this-command))
      (nvp-theme-dark)
    (nvp-theme-light)
    (setq this-command nil)))

(global-set-key (kbd "<f2> t s") 'nvp-theme-switch)

(provide 'nvp-theme)
;;; nvp-theme.el ends here
