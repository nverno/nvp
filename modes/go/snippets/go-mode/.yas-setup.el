;; -*- lexical-binding: t; -*-
(defun nvp-go-mirror-var ()
  (let ((str (or (yas-text) "")))
    (if (string-prefix-p "*" str)
        (concat "(" str ")")
      str)))

(defun nvp--go-type-var (s)
  (when s
    (downcase
     (let (case-fold-search)
       (replace-regexp-in-string "[a-z0-9]+" "" s t)))))
