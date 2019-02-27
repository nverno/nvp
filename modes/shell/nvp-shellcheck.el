;;; nvp-shellcheck.el --- shellcheck compilation -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-07 08:16:31>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Maintainer: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/shell-tools
;; Package-Requires: 
;; Created: 24 January 2019

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
  (require 'nvp-macro))
(declare-function xterm-color-colorize-buffer "xterm-color")
(declare-function nvp-compile "nvp-compile")

;;;###autoload
(defun nvp-shellcheck ()
  "Check current buffer with shellcheck."
  (interactive)
  (nvp-with-process "shellcheck" :proc-args ((buffer-file-name))
    :on-failure (progn
                  (pop-to-buffer "*shellcheck*")
                  (xterm-color-colorize-buffer)
                  (view-mode))))

;;;###autoload
(defun nvp-shellcheck-compile ()
  "Run shellcheck on current buffer with output to compilation buffer."
  (interactive)
  (let* ((compile-command (concat "shellcheck " (buffer-file-name)))
         (compilation-buffer-name-function
          #'(lambda (_m) (concat "*shellcheck: " (buffer-file-name) "*")))
         (funcs compilation-finish-functions)
         (kill-func #'(lambda (&rest _ignored)
                        (nvp-use-local-bindings ("q" . kill-this-buffer))
                        ;; reset compilation-finish-functions
                        (setq compilation-finish-functions funcs))))
    (setq compilation-finish-functions kill-func)
    (nvp-compile)))

(defun nvp-shellcheck-compilation-setup ()
  "Add compilation regexp for shellcheck output."
  (when (not (assoc 'shellcheck compilation-error-regexp-alist-alist))
    (let ((re '(shellcheck "In \\([^ \t\n]+\\) line \\([0-9]+\\)" 1 2)))
      (push (car re) compilation-error-regexp-alist)
      (push re compilation-error-regexp-alist-alist))))

(with-eval-after-load 'compile
  (nvp-shellcheck-compilation-setup))

(provide 'nvp-shellcheck)
;;; nvp-shellcheck.el ends here
