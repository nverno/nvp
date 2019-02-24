;;; nvp-elisp-test.el --- elisp tests -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/elisp-utils
;; Last modified: <2019-01-14 23:58:25>
;; Package-Requires: 
;; Created:  2 December 2016

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
(require 'nvp-project)
(require 'nvp-test)
(declare-function ert-run-tests-interactively "ert")

;; -------------------------------------------------------------------
;;; Util 

(defun nvp-elisp--run-tests (&optional prefix regexp)
  (eval-buffer)
  (ert-run-tests-interactively
   (or (and prefix (format "%s--test" prefix))
       regexp
       ".*")))

;; -------------------------------------------------------------------
;;; Commands 

;; find and run associated ert tests
;;;###autoload
(defun nvp-elisp-run-tests (arg)
  (interactive "P")
  (nvp-with-project (:test-re ".*tests?\.el")
    (nvp-with-test 'local 'create nil nil nil
      (nvp-elisp--run-tests nil (if arg (read-string "Test regexp: "))))))

(provide 'nvp-elisp-test)
;;; nvp-elisp-test.el ends here
