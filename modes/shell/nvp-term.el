;;; nvp-term.el --- term mode -*- lexical-binding: t; -*-

;; Last modified: <2019-03-28 18:53:51>
;; Created: 13 January 2019

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

(provide 'nvp-term)
;;; nvp-term.el ends here
