;;; nvp-fortran.el ---  -*- lexical-binding: t; -*-

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
