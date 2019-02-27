;;; kerl.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-15 10:39:08>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/erlang-tools
;; Package-Requires: 
;; Created: 22 December 2016

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

;; [![Build Status](https://travis-ci.org/nverno/erlang-tools.svg?branch=master)](https://travis-ci.org/nverno/erlang-tools)

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib)
  (defvar eshell-path-env))

;; non-nil kerl speaks about changes
(defvar kerl-verbose t)

;; hooks run after [de]activation
(defvar kerl-after-activation-hook ())

;; -------------------------------------------------------------------
;;; Utils

;; alist of kerl installs . paths
(defun kerl-installs ()
  (let (res)
    (with-temp-buffer
      (call-process "kerl" nil t nil "list" "installations")
      (goto-char (point-min))
      (while (re-search-forward
              "\\(^[^ ]+\\)[ \t]+\\([^\n\r]+\\)" nil 'move)
        (push (cons (match-string 1) (match-string 2)) res)))
    res))

;; remove kerl paths from env, update with new
(defun kerl-update-path (&optional new)
  (ignore-errors
    (start-process "kerl_deactivate" nil "kerl_deactivate"))
  (let* ((installs (kerl-installs))
         (paths (mapcar #'(lambda (x) (expand-file-name "bin" (cdr x)))
                        installs))
         (path (split-string (getenv "PATH") path-separator)))
    (when paths
      (setq path
            (cl-remove-if #'(lambda (s) (cl-member s paths :test 'string=))
                          path)))
    (when new
      (setq path (cons (expand-file-name
                        "bin" (cdr (assoc-string new installs)))
                       path)))
    (setq exec-path path)
    (setenv "PATH" (mapconcat 'identity exec-path path-separator))
    (setq eshell-path-env (getenv "PATH"))
    (run-hooks 'kerl-after-activation-hook)
    (when kerl-verbose
      (message "[kerl] %sctivated %s" (if new "A" "Dea") (or new "")))))

;; -------------------------------------------------------------------
;;; [De]Activate build

;;;###autoload
(defun kerl-activate (build)
  "Activate kerl build."
  (interactive
   (list (ido-completing-read "Activate: " (kerl-installs))))
  (and build (kerl-update-path build)))

;;;###autoload
(defun kerl-deactivate ()
  "Deactivate kerl build."
  (interactive)
  (kerl-activate nil))

(provide 'kerl)
;;; kerl.el ends here
