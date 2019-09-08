;;; nvp-sed.el --- sed -*- lexical-binding: t; -*-
;;; Commentary:
;; TODO:
;; - info-lookup-add-help
;; - see helm-info
;;; Code:
(eval-when-compile (require 'nvp-macro))
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
