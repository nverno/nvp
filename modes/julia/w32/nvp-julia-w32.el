;;; nvp-julia-w32.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-21 01:17:58>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/w32
;; Package-Requires: 
;; Created: 21 February 2019

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
  (require 'nvp-macro))
(require 'nvp-julia)

(eval-when-compile
  (make-symbolic-link
   (expand-file-name "julia/w32/repl_init.jl" nvp/install) "repl_init.jl" 'ok))

(or (getenv "JULIA_HOME")
    (setenv "JULIA_HOME" (file-name-directory (nvp-program "julia"))))

;; -------------------------------------------------------------------
;;; REPL 

(eval-when-compile
  (defvar julia-arguments)
  (defvar inferior-julia-args))

;; repl loading on windows -- this is years old, probably works now
(when load-file-name
  (let ((init (expand-file-name "w32/repl_init.jl" (nvp-package-root))))
    (setq inferior-julia-args (format "-i -L%s" init)
          julia-arguments `("-i" "-L" ,init))))

;; eldoc didn't used to work
(define-advice julia (:around (old-fn &rest args) "disable-eldoc")
  (let ((ess-use-eldoc 'script-only))
    (apply old-fn args)))

(provide 'nvp-julia-w32)
;;; nvp-julia-w32.el ends here
