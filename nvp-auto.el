;;; nvp-auto.el --- lesser used autos -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-15 12:33:40>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 14 February 2019

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
  (require 'hydra))
(require 'nvp)

;;; Generate wrapper functions to call locals
;;;###autoload(autoload 'nvp-check-buffer "nvp-auto")
;;;###autoload(autoload 'nvp-repl-switch "nvp-auto")
;;;###autoload(autoload 'nvp-test "nvp-auto")
;;;###autoload(autoload 'nvp-tag "nvp-auto")
(nvp-wrapper-fuctions
 (nvp-check-buffer-function . nil)
 (nvp-repl-switch-function  . nil)
 (nvp-test-function         . nil)
 (nvp-tag-function          . nil))

;; #<marker at 11387 in lisp.el.gz>
;; FIXME: account for continued expansion
;;;###autoload
(defun nvp-mark-defun (&optional _first-time &rest _rest)
  "Mark defuns, expanding successively."
  (interactive "P")
  (setq mark-active nil)
  (beginning-of-defun)
  (mark-defun)
  (comment-forward (point-max)))

;;;###autoload(autoload 'nvp-hydra-goto-line/goto-line "nvp-auto")
(nvp-hydra-set-property 'nvp-hydra-goto-line)
(defhydra nvp-hydra-goto-line (goto-map) "line"
  ("g" goto-line "go")
  ("b" (push-mark (car mark-ring) nil 'activate) "mark to start")
  ("m" set-mark-command "mark" :bind nil)
  ("p" (set-mark-command 1) "pop" :bind nil)
  ("e" exchange-point-and-mark "exchange")
  ("q" nil "quit"))

(provide 'nvp-auto)
;;; nvp-auto.el ends here
