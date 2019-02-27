;;; ocaml-abbrev ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/ocaml-tools
;; Package-Requires: 
;; Created: 14 December 2016

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

;; get functions and type signatures for MODULE
(defun ocaml-abbrev-module-sig (module)
  (with-temp-buffer
    (let ((proc (start-process "ocaml" (current-buffer) "ocaml"))
          res)
      (process-send-string
       proc (format "module type S = module type of %s;;\n" module))
      (sit-for 0.1)
      (goto-char (point-min))
      (when (re-search-forward "^\\s-*sig" nil 'move)
        (forward-line)
        (while (not (eobp))
          (when (looking-at
                 (nvp-concat
                  "[ \t]*\\(?:[[:alnum:]]+\\)[ \t]+\\([A-Za-z_0-9]+\\)"
                  "[ \t]*:[ \t]*\\([^\n\r]+\\)$"))
            (push (cons (match-string 1) (match-string 2)) res))
          (forward-line)))
      res)))

(provide 'ocaml-abbrev)
;;; ocaml-abbrev.el ends here
