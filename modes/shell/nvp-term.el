;;; nvp-term.el --- term mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'term)

(defun nvp-term-esc ()
  (interactive)
  (term-send-raw-string "\e"))

(defun nvp-term-eof ()
  (interactive)
  (if (or (eobp)
          (and (= 1 (- (point-max) (point)))
               (= 32 (char-before))))
      (term-send-eof)
    (delete-char 1)))

(defun nvp-term-toggle-mode ()
  (interactive)
  (if (term-in-char-mode)
      (term-line-mode)
    (term-char-mode)))

(provide 'nvp-term)
;;; nvp-term.el ends here
