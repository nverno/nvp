;;; nvp-installer-ext.el --- Install external deps -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-20 18:32:30>
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
(require 'make-mode)
(require 'nvp)

(cl-defstruct (nvp-mode (:constructor nvp-mode--make)
                        (:copier nil))
  "Mode configuration and dependencies."
  depends                               ;modes on which it depends
  pkgs                                  ;pkgs using default manager
  external                              ;external dependencies
  paths                                 ;load-paths
  )

;; -------------------------------------------------------------------
;;; Manage external dependencies

(cl-defstruct (nvp-install-ext (:constructor nvp-install-ext--make)
                               (:copier nil))
  "Struct to hold external installer info."
  (mode :read-only t) (makefile :read-only t) (targets :read-only t))

(cl-defun nvp-install-ext-make (targets &key mode (makefile nvp-install-makefile))
  (nvp-install-ext--make :targets targets :mode mode :makefile makefile))

(defun nvp-install-ext-help (ext)
  (nvp-with-results-buffer (help-buffer)
    (call-process-shell-command
     (concat "make -C " (file-name-directory (nvp-install-ext-makefile ext)) " help")
     nil t t)))

(setq tst (nvp-install-ext-make '("default") :mode "elisp"))
;; (defun nvp-installer--call (installer &rest args)
;;   )

;; (defun nvp-installer--help (&optional makefile)
;;   "Return MAKEFILE help doc."
;;   (with-output-to-temp-buffer)
;;   )

(defun nvp-installer--make-targets (&optional makefile)
  "List available targets in MAKEFILE, defaulting to `nvp-installer-makefile'."
  (with-temp-buffer
    (insert-file-contents (or makefile nvp-install-makefile))
    (makefile-pickup-targets)
    makefile-target-table))

;; (defun nvp-installer-make-target (&optional makefile &rest targets)
;;   "Install MAKEFILE TARGETS."
;;   (interactive (list nvp-installer-makefile)))


(provide 'nvp-installer-ext)
;;; nvp-installer-ext.el ends here
