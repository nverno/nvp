;;; ocaml-debug ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/ocaml-tools
;; Package-Requires: 
;; Created: 13 December 2016

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
(require 'ocaml-tools)
(require 'ocamldebug)

;; ocamldebug.el doesn't define a prefix key like gud-gdb
(defvar ocaml-debug-key-prefix (kbd "<f2> d"))

(defmacro ocaml-debug-bindings (&rest bindings)
  (declare (indent defun))
  `(eval-after-load 'ocamldebug
     (progn
       ,@(cl-loop for (name . k) in bindings
            collect `(define-key tuareg-mode-map
                       (vconcat ocaml-debug-key-prefix ,k)
                       ',(intern (concat "ocamldebug-" name)))))))

(ocaml-debug-bindings
  ("run"       . "\C-r")
  ("reverse"   . "\C-v")
  ("last"      . "\C-l")
  ("backtrace" . "\C-t")
  ("open"      . "\C-o")
  ("close"     . "\C-c")
  ("finish"    . "\C-f")
  ("print"     . "\C-p")
  ("next"      . "\C-n")
  ("up"        . "<")
  ("down"      . ">")
  ("break"     . "\C-b"))

(defun ocaml-tools-debug-help ()
  (interactive)
  (browse-url
   "https://caml.inria.fr/pub/docs/manual-ocaml/debugger.html"))

;; compile and debug
;;;###autoload
(defun ocaml-tools-compile-and-debug ()
  (interactive)
  (funcall-interactively 'ocaml-tools-compile '("-g" "-o a.out"))
  (ocamldebug "a.out"))

(provide  'ocaml-debug)
;;; ocaml-debug.el ends here
