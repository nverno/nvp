;;; nvp-fish.el --- fish shell helpers -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/shell-tools
;; Last modified: <2019-01-24 17:53:16>
;; Package-Requires: 
;; Created:  4 November 2016

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
  (defvar company-keywords-alist)
  (defvar fish-builtins)
  (defvar fish-keywords))
(require 'nvp-shell)

;; add fish keywords / builtins for company completion
(defun nvp-fish-company-keywords ()
  (when (bound-and-true-p company-keywords-alist)
    (unless (assq 'fish-mode company-keywords-alist)
      (setq company-keywords-alist
            (cons
             (cons 'fish-mode (append fish-builtins fish-keywords))
             company-keywords-alist)))))

(with-eval-after-load 'company
  (nvp-fish-company-keywords))

(provide 'nvp-fish)
;;; nvp-fish.el ends here
