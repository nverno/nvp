;;; nvp-sed.el --- sed -*- lexical-binding: t; -*-

;; Last modified: <2019-03-25 03:29:11>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 15 March 2019

;;; Commentary:
;; TODO:
;; - info-lookup-add-help
;; - see helm-info
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

(defun nvp-sed-setup-locals ()
  (setq imenu-generic-expression '((nil "^:\\([^ \n\f\t]+\\)" 1))))

(provide 'nvp-sed)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-sed.el ends here
