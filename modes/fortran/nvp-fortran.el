;;; nvp-fortran.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-27 15:30:12>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/fortran
;; Created: 27 March 2019

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-compile)
  (require 'cl-lib))

;; -founds-check: check for array access out of bounds
(nvp-make-or-compile-fn nvp-fortran-compile
    (:cmake-action nil
     :default-prompt (read-from-minibuffer "Compiler flags: "))
  (let* ((flags (or args
                    "-Wall -Wextra -fbounds-check -g -std=f95 -Ofast"))
         (file (file-name-nondirectory buffer-file-name))
         (out (file-name-sans-extension file))
         (compile-command
          (format "%s %s -o %s%s %s" (nvp-program "gfortran")
                  flags out (nvp-with-gnu/w32 "" ".exe") file)))
    (nvp-compile)))

(provide 'nvp-fortran)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-fortran.el ends here
