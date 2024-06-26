;;; nvp-fortran.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-compile))

;; -founds-check: check for array access out of bounds
(defun nvp-fortran-compile (prefix)
  "Compile using file-local variables, make, cmake, or build compile command."
  (interactive "P")
  (unless (nvp-compile-maybe-default prefix)
    (let* ((file (file-name-nondirectory buffer-file-name))
           (out (concat (file-name-sans-extension file) ".out"))
           (compile-command
            (format "%s -Wall -Wextra -fbounds-check -g -std=f95 -Ofast -o %s%s %s"
                    (nvp:program "gfortran")
                    out (nvp:with-gnu/w32 ".out" ".exe") file)))
      (call-interactively #'nvp-compile))))

(provide 'nvp-fortran)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-fortran.el ends here
