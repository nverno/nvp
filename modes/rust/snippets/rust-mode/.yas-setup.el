;; -*- lexical-binding: t; -*-
(defun $rust-iter (_type)
  (pcase (yas-text)
    ("u8" "bytes()")
    ("char" "chars()")
    (_ "iter()")))
