;;; nvp-flex.el --- flex helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-compiler)
(declare-function nvp-env-add "nvp-env")

;;; Environment

;; add flex source/headers to CPATH
(defun nvp-flex-setenv ()
  (interactive)
  (nvp-env-add "CPATH" (expand-file-name "flex/src" (getenv "DEVEL"))))

;; -------------------------------------------------------------------
;;; Commands

;; replace symbol at point with case-insensitve regex version
;; eg. thing => [Th][Hh][Ii][Nn][Gg]
(defun nvp-flex-uncasify-symbol ()
  (interactive)
  (let* ((bnds (bounds-of-thing-at-point 'symbol))
         (sym (buffer-substring (car bnds) (cdr bnds)))
         (str (mapconcat 'identity
                         (cl-mapcar (lambda (a b)
                                      (concat "[" (char-to-string a)
                                              (char-to-string b) "]"))
                                    (upcase sym) (downcase sym))
                         "")))
    (delete-region (car bnds) (cdr bnds))
    (insert str)))

(provide 'nvp-flex)
;;; nvp-flex.el ends here
