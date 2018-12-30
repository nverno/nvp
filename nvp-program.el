;;; nvp-program ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 17 December 2018

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
  (require 'nvp-macro))

;; possible local locations
(defvar nvp-install-local-locations '("~/.local/bin/" "/usr/local/bin/"))

;; locate program in local locations
;;;###autoload
(defun nvp-install-find-local-program (program &optional path)
  (nvp-with-gnu/w32
      (cl-loop for p in (if path (cons path nvp-install-local-locations)
                          nvp-install-local-locations)
         do (let ((f (expand-file-name program p)))
              (and (file-exists-p f)
                   (cl-return f))))
    (bound-and-true-p (intern (concat "nvp-" program "-program")))))

(provide 'nvp-program)
;;; nvp-program.el ends here