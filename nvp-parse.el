;;; nvp-parse.el --- Generics for parse -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-22 00:53:15>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created:  7 February 2019

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
;; #<marker at 10467 in which-func.el.gz>
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))

;; -------------------------------------------------------------------
;;; utils

(eval-when-compile
 (defmacro nvp--parse-ensure-imenu ()
   "Create imenu index alist if possible."
   `(ignore-errors
      (when (and (fboundp 'imenu--make-index-alist) (null imenu--index-alist))
        (imenu--make-index-alist 'noerror)))))

;; -------------------------------------------------------------------
;;; Generics

;; default just tries to use imenu
(cl-defgeneric nvp-parse-function-names (&optional buffer-or-file &rest _args)
  "Default method to gather function names from current buffer or BUFFER-OR-FILE."
  (nvp--parse-ensure-imenu)
  ;; just loops through alist and gathers names
  (let ((buff (if (not buffer-or-file) (current-buffer)
                (if (buffer-live-p buffer-or-file)
                    buffer-or-file
                  (find-file-noselect buffer-or-file)))))
    (set-buffer buff)
    (when-let ((objs (cdr (imenu--make-index-alist))))
      (cl-loop for (func . loc) in objs
         collect func))))

(defun nvp-list-flatten (lst)
  "Flatten nested list."
  (declare (pure t) (side-effect-free t))
  (if (and (listp lst) (listp (cdr lst)))
      (apply #'append (mapcar (lambda (x) (nvp-list-flatten x)) lst))
    (list lst)))

(defun nvp-imenu-sort-by-relative-position (pos item1 item2)
  "Sort items such that closest elements to POS in buffer are first.
Assumes the list is flattened and only elements with markers remain.
Return non-nil if ITEM1's position is closer than ITEM2's."
  (< (abs (- pos (cdr item1)))
     (abs (- pos (cdr item2)))))

(setq tst
      (cl-sort
       (cl-remove-if-not
        (lambda (x) (and (listp x) (number-or-marker-p (cdr x)))) tst)
          (apply-partially #'nvp-imenu-sort-by-relative-position (point-marker))))

(setq tst (nvp-list-flatten imenu--index-alist))

;; sort by imenu--relative-position to find closest match in buffer
;; like which-func - attempt with imenu and add-log
(cl-defgeneric nvp-parse-current-function (&rest _args)
  "Default method to get name of function containing point."
  (ignore-errors
    (when (and (fboundp 'imenu--make-index-alist) (null imenu--index-alist))
      (imenu--make-index-alist 'noerror))
    ;; imenu alists can be nested under headers:
    ;; ("Header" ("name" . marker)) or ("name" . marker)
    (when-let ((alist (bound-and-true-p imenu--index-alist)))
      (let (())))))

(provide 'nvp-parse)
;;; nvp-parse.el ends here
