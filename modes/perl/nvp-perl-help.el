;;; nvp-perl-help.el --- help-at-point -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/perl-tools
;; Package-Requires: ((pos-tip "0"))
;; Created:  9 November 2016

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
(require 'nvp-perl)
(require 'cperl-mode)
(require 'man)
(require 'pos-tip)

(defvar nvp-perl-help-max-lines 20)

;; -------------------------------------------------------------------
;;; Util

(eval-when-compile
 ;; read input in various ways
 (defmacro nvp-perl-help-read (prompt &optional thing)
   (declare (indent defun))
   (pcase thing
     ((pred stringp)
      `(read-from-minibuffer ,prompt ,thing))
     (`(quote ,sym)
      (if (eq sym 'module)
          `(ido-completing-read ,prompt (nvp-perl-modules))
        `(ido-completing-read ,prompt (symbol-value ,thing))))
     ((and (pred symbolp)
           (guard (equal "module" (symbol-name thing))))
      `(ido-completing-read "Module: " (nvp-perl-modules)))
     ((pred consp)
      `(ido-completing-read ,prompt ,thing))
     (_ `(read-from-minibuffer ,prompt)))))

;; -------------------------------------------------------------------
;;; Perldoc

;; cperl-perldoc with completing read for module names
;; with prefix, lookup online at search.cpan.org
;;;###autoload
(defun nvp-perl-help-perldoc (arg &optional thing)
  (interactive "P")
  (let ((thing
         (or thing
             (and (member arg '((4) (64)))
                  (nvp-perl-help-read "Module: " 'module))
             (thing-at-point 'perl-module)
             (thing-at-point 'perl-variable)
             (thing-at-point 'symbol)
             (nvp-perl-help-read "Module: " 'module))))
    (if (member arg '((16) (64)))
        (browse-url (format "http://search.cpan.org/perldoc?%s" thing))
      (cperl-perldoc thing))))

;; old perldoc on module
(defun nvp-perl-help-perldoc-module ()
  (interactive)
  (let ((sym (nvp-perl-help-read "Perldoc: "
               (thing-at-point 'perl-module))))
    (condition-case nil
        (find-file 
         (with-temp-buffer
           (shell-command (format "perldoc -l %s" sym) (current-buffer))
           (goto-char (point-min))
           (and (looking-at "\\(.+\\)")
                (match-string 0))))
      (error
       (user-error "Could not read the module path!")))))

;;; Cpan / Cpanm

;; Search cpan online for query.
;;;###autoload
(defun nvp-perl-help-cpan-online (query)
  (interactive "sQuery: ")
  (browse-url
   (format "http://search.cpan.org/search?query=%s&mode=all" query)))

;; -------------------------------------------------------------------
;;; TODO: help-at-point

;;;###autoload
(defun nvp-perl-help-at-point (_obj)
  (interactive "P")
  (or (x-hide-tip)
      (let* ((sym (cperl-word-at-point))
             (case-fold-search nil)
             ;; `cperl-perldoc'
             (is-func (and (string-match-p "^[a-z]+$" sym)
                           (string-match-p (concat "^" sym "\\>")
                                           (documentation-property
                                            'cperl-short-docs
                                            'variable-documentation))))
             (Man-switches "")
             (Man-notify-method 'quiet)
             (manual-program (if is-func "perldoc -f" "perldoc")))
        (Man-getpage-in-background sym))))

(provide 'nvp-perl-help)
;;; nvp-perl-help.el ends here
