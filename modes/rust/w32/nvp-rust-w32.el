;;; nvp-rust-w32.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-21 01:30:50>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/w32
;; Package-Requires: 
;; Created: 21 February 2019

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
  (nvp-local-vars))
(require 'nvp-rust)

;; install on windows with chocolatey
(defun nvp-rust-w32-install (install)
  (interactive "P")
  (w32-shell-execute
   "runas" "powershell"
   (concat "-c choco " (if install "install" "upgrade") " -y rust")))

;; FIXME: use nvp-env-exec-add
;; add directory DIR to exec-path
(defun nvp-rust-w32-exec-path (dir)
  (let ((path
         (append `(,dir) (split-string (getenv "PATH") path-separator))))
    (setenv "PATH" (mapconcat 'identity path path-separator))
    (setq exec-path path)))

;; FIXME: use nvp-env-setenv!
;; permanently set environment variable (for user)
(defun nvp-rust-w32-setenv! (env-var value)
  (let* ((ps (expand-file-name "w32/Set-Env.ps1" nvp/install))
         (val (replace-regexp-in-string "/" "\\\\" value)))
    (w32-shell-execute
     "runas" "powershell" (format " -File %s \"%s\" \"%s\"" ps env-var val))
    ;; update env for current session as well
    (if (string= (upcase env-var) "PATH")
        (nvp-rust-w32-exec-path val)
      (setenv env-var val))))

(provide 'nvp-rust-w32)
;;; nvp-rust-w32.el ends here
