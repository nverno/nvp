;; -*- lexical-binding: t; -*-
(defun nvp-go-mirror-var ()
  (let ((str (or (yas-text) "")))
    (if (string-prefix-p "*" str)
        (concat "(" str ")")
      str)))
