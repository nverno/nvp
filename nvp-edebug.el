;;; nvp-edebug.el --- edebugging helper -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-20 16:59:12>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 20 February 2019

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
  (require 'nvp-macro)
  (require 'which-key nil t))
(require 'edebug)

;; ;;;###autoload
;; (defun nvp-edebug-eval-hook (old-fn &rest args)
;;   (minibuffer-with-setup-hook ()
;;     (nvp-minibuffer-eval-hook)))

;;;###autoload
(advice-add 'edebug-eval-expression :after #'nvp-minibuffer-eval-hook)

;;;###autoload
(defun nvp-edebug-help ()
  (interactive)
  (require 'which-key)
  (setq-local which-key-idle-delay 0.1)
  (setq-local which-key-popup-type 'side-window)
  (which-key-show-keymap 'edebug-mode-map)
  (which-key-mode -1))

(provide 'nvp-edebug)
;;; nvp-edebug.el ends here
