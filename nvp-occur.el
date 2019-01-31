;;; nvp-occur.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-01-31 14:34:07>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Maintainer: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 31 January 2019

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
  (require 'cl-lib)
  (require 'replace))
(autoload 'nvp-buffer-matching-mode "nvp-buffer")

;; Return the contents of the region or symbol.
(defun nvp-occur-region-str-or-symbol ()
  (if (region-active-p)
      (buffer-substring-no-properties
       (region-beginning)
       (region-end))
    (let ((sym (thing-at-point 'symbol)))
      (when (stringp sym)
        (regexp-quote sym)))))

;; Call `occur' with a sane default.
;;;###autoload
(defun nvp-occur-dwim ()
  (interactive)
  (push (nvp-occur-region-str-or-symbol) regexp-history)
  (call-interactively 'occur)
  (if (get-buffer "*Occur*")
      (switch-to-buffer-other-window "*Occur*")))

;; https://www.masteringemacs.org/article/searching-buffers-occur-mode
;; Show all lines matching REGEXP in buffers withe the same 
;; `major-mode'.
;;;###autoload
(defun nvp-multi-occur-in-this-mode ()
  (interactive)
  (multi-occur
   (nvp-buffer-matching-mode major-mode)
   (car (occur-read-primary-args))))

(provide 'nvp-occur)
;;; nvp-occur.el ends here
