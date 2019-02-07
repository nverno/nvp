;;; nvp-yas-auto.el --- yas helpers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-07 14:43:22>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created:  7 February 2019

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
(require 'nvp)
(require 'yasnippet)

;;;###autoload
(defun nvp-yas-reload-all ()
  "Reload modes' snippet tables."
  (interactive)
  (unless (member nvp-snippet-dir yas-snippet-dirs)
    (push nvp-snippet-dir yas-snippet-dirs))
  (cl-loop for dir in yas-snippet-dirs
     do (yas-load-directory dir)))

;; compile snippets, optionally compile all snippet subdirs in site-lisp addons
;;;###autoload
(defun nvp-yas-compile (&optional all)
  "Compile snippets in default location.
Optionally, compile ALL snippets including subdirs in site-lisp packages."
  (interactive "P")
  (let ((yas-snippet-dirs
         (cons nvp/snippet
               (and all (directory-files-recursively nvp/site "snippets" 'dirs)))))
    (mapc #'yas-recompile-all yas-snippet-dirs)))

;; -------------------------------------------------------------------
;;; Active expansion

;;;###autoload
(defun nvp-yas-end-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas-active-snippets)))
	 (position (yas--field-end (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-end-of-line 1)
      (goto-char position))))

;;;###autoload
(defun nvp-yas-start-of-active-field ()
  (interactive)
  (let* ((snippet (car-safe (yas-active-snippets)))
	 (position (yas--field-start
                    (yas--snippet-active-field snippet))))
    (if (= (point) position)
	(move-beginning-of-line 1)
      (goto-char position))))

(provide 'nvp-yas-auto)
;;; nvp-yas-auto.el ends here
