;;; nvp-sed.el --- sed -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-15 18:22:04>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 15 March 2019

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'sed-mode nil t)

(defun nvp-sed-help ()
  "Go to help place."
  (interactive)
  (let ((url "https://www.gnu.org/software/sed/manual/sed.html"))
    (browse-url url)))

(provide 'nvp-sed)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-sed.el ends here
