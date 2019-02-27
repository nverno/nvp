;;; nvp-r-package.el --- package helpers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/r-tools
;; Last modified: <2019-02-14 05:18:35>
;; Package-Requires: 
;; Created: 24 January 2017

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
(require 'nvp-r)

;; ;;;###autoload
;; (defun nvp-r-package-create (name)
;;   (interactive (list (read-from-minibuffer "Package name: ")))
;;   (nvp-with-script (expand-file-name "script/tools.sh" (nvp-package-root))
;;     `(("create" ,name))))

(provide 'nvp-r-package)
;;; nvp-r-package.el ends here
