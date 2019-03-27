;;; exenv.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-26 21:07:21>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/erlang-tools
;; Package-Requires: 
;; Created: 24 December 2016

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
(autoload 'string-trim "subr-x")

(defvar exenv-dir "~/.exenv")

;; -------------------------------------------------------------------
;;; List

(eval-when-compile
  (defvar tabulated-list-format)
  (defvar tabulated-list-entries))
(declare-function tabulated-list-init-header "tabulated-list")
(declare-function tabulated-list-print "tabulated-list")
(declare-function tabulated-list-get-entry "tabulated-list")

(defun exenv-available-installs ()
  "Exenv list available installs as tabulated list."
  (interactive)
  (let ((installs
         (mapcar #'(lambda (s)
                     (let ((str (string-trim s)))
                       (list str (vector str))))
                 (cdr (process-lines "exenv" "install" "--list"))))
        (buff (get-buffer-create "*exenv*")))
    (with-current-buffer buff
      (setq tabulated-list-format [("Build" 20 nil)])
      (setq tabulated-list-entries installs)
      (exenv-mode)
      (tabulated-list-print)
      (pop-to-buffer (current-buffer)))))

(defun exenv-install (id)
  (interactive (list (aref (tabulated-list-get-entry) 0)))
  (nvp-with-process-log
    (start-process-shell-command
     "exenv" "*exenv*" (concat "exenv install " id))
    :on-error (pop-to-buffer (current-buffer))
    :on-success (view-mode))
  (display-buffer "*exenv*"))

(defvar exenv-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "RET") 'exenv-install)
    km))

(define-derived-mode exenv-mode tabulated-list-mode
  "Exenv"
  "Commands: 
\\{exenv-mode-map}"
  (tabulated-list-init-header))

(provide 'exenv)
;;; exenv.el ends here
