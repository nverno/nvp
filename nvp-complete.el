;;; nvp-complete ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
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

;;--- Command Line Switches ------------------------------------------
;; Completion for command line arguments

;; bind dynamically
(defvar-local nvp-complete--switch-program nil)
(defvar-local nvp-complete--switch-cache nil)
(defvar-local nvp-complete--switch-args nil)

;; gather command line switches for completion
(defun nvp-complete--switch ()
  (or (and (bound-and-true-p nvp-complete--switch-cache)
           (symbol-value nvp-complete--switch-cache))
      (with-temp-buffer
        (apply 'call-process nvp-complete--switch-program
               nil (current-buffer) nil
               (or nvp-complete--switch-args '("--help")))
        (goto-char (point-min))
        (let (res)
          (while (re-search-forward "^\\s-*\\(-[a-zA-Z0-9]+\\)" nil t)
            (push (match-string 1) res)
            (while (re-search-forward "\\(-[a-zA-Z0-9]+\\)"
                                      (line-end-position) t)
              (push (match-string 1) res)))
          (if nvp-complete--switch-cache
              (set nvp-complete--switch-cache (nreverse res))
            res)))))

;; completion at point for command line switch
(defun nvp-complete-switch-completion ()
  (let ((bnds (bounds-of-thing-at-point 'symbol)))
    (if (and bnds
             (eq ?- (char-after (car bnds))))
        (list (car bnds) (cdr bnds) (nvp-complete--switch)))))

;; read from minibuffer with completion for command line switches
;;;###autoload
(defun nvp-complete-read-switch (prompt &optional initial-contents)
  (let ((minibuffer-completing-symbol nil))
    (minibuffer-with-setup-hook
     (lambda ()
       (add-hook 'completion-at-point-functions
                 'nvp-complete-switch-completion nil 'local))
     (read-from-minibuffer prompt initial-contents
                           read-expression-map nil
                           'read-expression-history))))
(provide 'nvp-complete)
;;; nvp-complete.el ends here
