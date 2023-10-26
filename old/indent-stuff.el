;; -*- lexical-binding: t; -*-

;; make indentation based regexp
(defun nvp-indent-regexp ()
  (concat "^\\(?:[ \t]*$\\|"
          (buffer-substring
           (point)
           (save-excursion
             (progn (back-to-indentation) (point))))
          "\\)"))
