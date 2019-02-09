;;; nvp-read.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-02-09 07:06:31>
;; Package-Requires: 
;; Created: 29 November 2016

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
(autoload 'eldoc-minibuffer-message "eldoc")

;;; TODO:
;; - read w/ popup help: see `register-read-with-preview'

;;;###autoload
(defun nvp-read-with-message (prompt &optional format-string &rest args)
  "Display message in mode-line while reading from minibuffer."
  (minibuffer-with-setup-hook
      (:append (lambda () (eldoc-minibuffer-message format-string args)))
    (read-from-minibuffer prompt)))

;;;###autoload
(defun nvp-read-obarray (prompt &optional regexp)
  "Completing read for obarray with optional REGEXP filter."
  (completing-read prompt obarray
                   (lambda (sym) (string-match-p regexp (symbol-name sym)))))

(provide 'nvp-read)
;;; nvp-read.el ends here
