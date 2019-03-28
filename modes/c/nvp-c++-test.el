;;; nvp-c++-test.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-27 23:37:17>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/c-tools
;; Package-Requires: 
;; Created: 17 January 2017

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
  (require 'nvp-c)
  (require 'nvp-c-test) ;; setup macros
  (defvar boost-test-abbrev-table))
(require 'nvp-test)
(autoload 'yas-expand "yasnippet")

;; -------------------------------------------------------------------
;;; Util

(eval-when-compile
 (defmacro nvp-with-c++-vars (&rest body)
   (declare (indent defun))
   `(nvp-with-project (:test-re ".*test.*\.cpp"
                       :root '("test" "tests" ".git" ".projectile"))
      ,@body))

 (defmacro nvp-c++-test--setup-buffer ()
   `(progn
      (setq-local local-abbrev-table boost-test-abbrev-table)
      (nvp-set-local-keymap :use t
        ("C-c C-c" . nvp-c++-test-run-unit-test)))))

;; -------------------------------------------------------------------
;;; Setup Test

(defun nvp-c++-test-init ()
  (insert "boost_init")
  (call-interactively 'yas-expand))

;; if new expand template for new test
(defun nvp-c++-test-setup-buffer (&optional new)
  (nvp-c++-test--setup-buffer)
  (when (or current-prefix-arg new)
    (goto-char (point-max))
    (insert "\nbatc")
    (call-interactively 'yas-expand)))

;;;###autoload(autoload 'nvp-project-c++-boost-setup "nvp-c++-test")
(nvp-define-project c++-boost
  :test-fmt "test_%s"
  :test-init-function 'nvp-c++-test-init
  :test-buffer-function 'nvp-c++-test-setup-buffer
  :test-run-unit-function 'nvp-c++-test-run-unit-test)

;; -------------------------------------------------------------------
;;; Commands 

(defun nvp-c++-test-help ()
  (interactive)
  (browse-url "https://github.com/jsankey/boost.test-examples/"))

(eval-and-compile
  (nvp-c-test--runner-fn nvp-c++-test-run-unit-test 'c++
    ;; flags
    "-std=c++14 -O3 -s"
    ;; link
    "-lboost_unit_test_framework"))

(provide 'nvp-c++-test)
;;; nvp-c++-test.el ends here
