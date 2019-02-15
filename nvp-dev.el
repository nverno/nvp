;;; nvp-dev.el --- devel helpers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-14 20:57:40>
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
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(nvp-declare "nadvice" advice-mapc advice-remove)
(nvp-declare "ert" ert-run-tests-interactively)

;;;###autoload
(defun nvp-ert-run-tests ()
  "Run ert tests.
With prefix ARG, prompt for selector."
  (interactive)
  (if (featurep 'ert)
      (call-interactively 'ert-run-tests-interactively)
    (user-error "`ert' must be loaded to run this function")))

;;;###autoload
(defun nvp-advice-remove-all (sym)
  "Remove all advice from SYM."
  (interactive "aFunction: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(provide 'nvp-dev)
;;; nvp-dev.el ends here
