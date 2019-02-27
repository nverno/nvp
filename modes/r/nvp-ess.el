;;; nvp-ess.el --- ESS -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/r-tools
;; Last modified: <2019-02-23 00:37:02>
;; Package-Requires: 
;; Created: 11 December 2018

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

;; stuff applicable across ESS dialects

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'ess)
(require 'ess-inf)
(require 'ess-utils)
(declare-function ess-mark-function-or-para "ess-mode")

;; Mark paragraphs, successively on repeated commands
;;;###autoload
(defun nvp-ess-mark-defun ()
  (interactive)
  (nvp--mark-defun                      ;FIXME
   ;; first mark
   (ess-mark-function-or-para)
   ;; repeated calls
   (condition-case nil
       (progn
         (forward-line 1)
         (end-of-defun)
         ;; (ess-end-of-function)
         )
     (error (forward-paragraph)))
   (point)))

;; -------------------------------------------------------------------
;;; Inferior

;;;###autoload
(defun nvp-ess-process-abort (arg)
  (interactive "P")
  (let ((proc (ess-get-process)))
    (when proc
      (if arg
          (kill-process proc)
        (interrupt-process proc)))))

;; Evaluate active region or line.
;;;###autoload
(defun nvp-ess-eval ()
  (interactive)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'ess-eval-region)
    (ess-eval-line-and-step)))

(provide 'nvp-ess)
;;; nvp-ess.el ends here
