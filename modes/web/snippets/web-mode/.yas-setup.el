;; -*- lexical-binding: t; -*-
(declare-function yas-text "yasnippet")

(defsubst web-yas-type ()
  (file-name-extension buffer-file-name))

(defun if-empty (empty &optional non-empty)
  (if (not (yas-text))
      empty
    (or non-empty "")))

(defun if-non-empty (non-empty)
  (if-empty "" non-empty))

(defun yas-quote (&optional str)
  (if str (concat str "\"") "\""))
