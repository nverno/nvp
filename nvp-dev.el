;;; nvp-dev.el --- elisp devel helpers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-21 08:13:35>
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
;;; TODO: function to remove all methods from generic
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(require 'help-mode)
(nvp-declare "nadvice" advice-mapc advice-remove)
(nvp-declare "ert" ert-run-tests-interactively)
(nvp-autoload "nvp-string" nvp-s-wrap nvp-s-center nvp-s-repeat)

(define-button-type 'help-marker
  :supertype 'help-xref
  'help-function (lambda (m) (pop-to-buffer (marker-buffer m)) (goto-char m))
  'help-echo (purecopy "mouse-2, RET: go to this marker"))

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

;; -------------------------------------------------------------------
;;; Syntax

;;;###autoload
(defun nvp-syntax-at-point (marker)
  "Message about syntax at point."
  (interactive (list (point-marker)))
  (set-buffer (marker-buffer marker))
  (let ((ppss (syntax-ppss marker))
        (help
         '("depth in parens."
           "character address of start of innermost containing list; nil if none."
           "character address of start of last complete sexp terminated."
           "non-nil if inside a string. \
(it is the character that will terminate the string, \
or t if the string should be terminated by a generic string delimiter.)"
           "nil if outside a comment, t if inside a non-nestable comment, \
else an integer (the current comment nesting)."
           "t if following a quote character."
           "the minimum paren-depth encountered during this scan."
           "style of comment, if any."
           "character address of start of comment or string; nil if not in one."
           "List of positions of currently open parens, outermost first."
           "When the last position scanned holds the first character of a \
(potential) two character construct, the syntax of that position, \
otherwise nil.  That construct can be a two character comment \
delimiter or an Escaped or Char-quoted character."
           ".... Possible further internal information used by \
‘parse-partial-sexp’.")))
    (help-setup-xref (list #'nvp-syntax-at-point marker)
                     (called-interactively-p 'interactive))
    (with-help-window (help-buffer)
      (princ
       (format "\n%s\n%s\n\n"
               (nvp-s-center 60 "Syntax at <marker>")
               (nvp-s-repeat 85 "~")))
      
      (cl-loop
         for i from 0 upto (length ppss)
         do
           (princ (format "%d) %S " i (nth i ppss)))
           (princ (format "%s" (nvp-s-wrap 45 (nth i help) "; ")))
           (terpri))
      (with-current-buffer standard-output
        (let ((inhibit-read-only t)
              (comment-start "; ")
              (fill-column 85)
              (comment-column 30))
          (goto-char (point-min))
          (search-forward "<marker>")
          (replace-match "")
          (help-insert-xref-button (format "%S" marker) 'help-marker marker)
          (forward-line 2)
          (while (not (eobp))
            (when (looking-at-p comment-start)
              (insert "|"))
            (comment-indent)
            (forward-line 1)))
        (hl-line-mode)))))

(provide 'nvp-dev)
;;; nvp-dev.el ends here
