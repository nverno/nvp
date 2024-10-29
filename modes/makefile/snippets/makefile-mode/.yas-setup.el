;; -*- lexical-binding: t; -*-
(declare-function s-replace "s")
(declare-function s-split-up-to "s")
(declare-function yas-text "yasnippet")

(defun nvp--make-defvar (&optional num)
  (when-let* ((txt (yas-text)))
    (upcase
     (s-replace
      "-" "_"
      (car (nthcdr (1- (or num 1)) (s-split-up-to "\\s-" txt (or num 1) t)))))))
