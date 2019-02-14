;;; nvp-installer-ext.el --- Install external deps -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-14 05:32:22>
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

;; - sentinels for external installs
;; - better logging
;; - `make-progress-reporter'

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'nvp-macro)
  (nvp-local-vars))
(require 'nvp)

;; -------------------------------------------------------------------
;;; Manage external dependencies

(cl-defstruct (nvp-installer (:constructor nvp-installer-make)
                             (:copier nil))
  "Struct to hold external installer info."
  location help targets pending buffer)

;; (defvar nvp-installer
;;   (nvp-installer-makefile-make
;;    :location (expand-file-name "Makefile" nvp/install)
;;    :help (nvp-installer--))
;;   "Installer instance.")

;; (defun nvp-installer--call (installer &rest args)
;;   )

(defun nvp-installer--help (&optional makefile)
  "Return MAKEFILE help doc."
  (with-output-to-temp-buffer)
  )
(defun nvp-installer--make-targets (&optional makefile)
  "List available targets in MAKEFILE, defaulting to `nvp-installer-makefile'."
  (with-temp-buffer
    (insert-file-contents (or makefile nvp-installer-makefile))
    (makefile-pickup-targets)
    makefile-target-table))

(defun nvp-installer-make-target (&optional makefile &rest targets)
  "Install MAKEFILE TARGETS."
  (interactive (list nvp-installer-makefile)))


(provide 'nvp-installer-ext)
;;; nvp-installer-ext.el ends here
