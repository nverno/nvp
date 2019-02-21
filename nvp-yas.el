;;; nvp-yas.el --- snippet helpers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-02-21 10:52:27>
;; Package-Requires: 
;; Created: 20 December 2016

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
  (nvp-local-vars))
(require 'nvp)
(require 'yasnippet)
(require 'nvp-comment)

(defalias 'yas-comment-string 'nvp-yas-comment)

;; -------------------------------------------------------------------
;;; Buffers / files

;; trimmed filename
(defsubst nvp-yas-bfn ()
  (nvp-bfn))

(defsubst nvp-yas-bfn-no-ext ()
  (nvp-bfn 'no-ext))

;; directory name
(defsubst nvp-yas-dfn ()
  (nvp-dfn))

;; current indentation
(defsubst nvp-yas-indent ()
  (current-indentation))

;; -------------------------------------------------------------------
;;; Padding / Headers

(defsubst nvp-yas-header (char &optional extra max)
  (let ((sw (string-width yas-text)))
    (make-string (if extra (min max (+ sw extra)) sw) char)))

;; add padding to `yas-text'
(defsubst nvp-yas-pad (char padmin padmax)
  (let* ((sw (string-width yas-text))
         (extra (max padmin (- padmax sw))))
    (make-string (/ (max 0 extra) 2) char)))

;; fill after yas-text with CHAR until PADMAX
(defsubst nvp-yas-pad-right (char padmax)
  (make-string (max 0 (- padmax (string-width yas-text))) char))

;; -------------------------------------------------------------------
;;; Syntax

;; yas-inside-string uses `font-lock-string-face'
(defsubst nvp-yas-inside-string ()
  (nth 3 (syntax-ppss)))

(defsubst nvp-yas-inside-comment ()
  (nth 4 (syntax-ppss)))

;; -------------------------------------------------------------------
;;; Args 

;; split argument STR by SEPS, eg. "a,b" => '("a" "b"). Strings are trimmed and
;; nulls are dropped.
;; if DEFAULTS is non-nil, split by "=" as well, eg.
;; "a,b=1," => '("a" ("b" "1"))
(defun nvp-yas-split-args (str &optional seps defaults)
  (let ((args (split-string str (or seps "[ \t]*,[ \t]*") t " ")))
    (if defaults
        (mapcar (lambda (s)
                  (let ((defs (split-string s "[ \t]*=[ \t]*" t " ")))
                    (if (= (length defs) 1) (car defs) defs)))
                args)
      args)))

;; get variable name from string of form "i = 1" or "int i = 1"
(defun nvp-yas-varname (str)
  (if (< (length str) 1)
      ""
   (let* ((str (car (split-string str "=" t " ")))
          (strs (split-string str nil t " ")))
     (or (cadr strs) (car strs)))))

;; -------------------------------------------------------------------
;;; Or patterns

(defsubst nvp-yas-or-values (str &optional seps)
  (split-string str (or seps "[\]\[|]") 'omit " "))

(provide 'nvp-yas)
;;; nvp-yas.el ends here
