;;; nvp-help.el --- help utils -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 24 November 2016
;; Last modified: 2019-01-14 11:38:12 noah

;;; Commentary:
;; Utility functions to parse help output
;; - parse man output
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'nvp)

;; -------------------------------------------------------------------
;;; Parsing Man output

;; regex to match man subentry
(defconst nvp-help--man-subentry-re
  "\\([^ \t]\\(?:[^ \t\n\r]\\| [^ \t\n\r]\\)+\\)")

;; make indentation based regexp
(defsubst nvp-help--man-indent ()
  (buffer-substring (point) (+ (point) (current-indentation))))

(defsubst nvp-help--man-indent-re ()
  (concat "^\\(?:[ \t]*$\\|" (nvp-help--man-indent) "\\)"))

(defsubst nvp-help--man-section-re ()
  (concat "^" (nvp-help--man-indent) "[^ \t\n\r]"))

;; return section from man doc
(defun nvp-help-man-string (section-re)
  (goto-char (point-min))
  (when (re-search-forward section-re nil 'move)
    ;; (forward-line)
    (beginning-of-line)
    (let* ((start (point))
           (section-re (nvp-help--man-section-re)))
      (forward-line)
      (while (not (or (eobp) (looking-at-p section-re)))
        (forward-line))
      (buffer-substring start (1- (point))))))

;; get switches from man section from START-RE to END-RE
(defun nvp-help-man-switches (section-re start-re end-re)
  (goto-char (point-min))
  (when (re-search-forward section-re)
    (forward-line)
    (let* ((indent-re (nvp-help--man-indent-re))
           (flag-re (concat indent-re nvp-help--man-subentry-re))
           (cont-re "\t[ \t]*\\|^$")
           res key start)
      (when (re-search-forward start-re)
        (beginning-of-line)
        (while (not (looking-at-p end-re))
          (if (not (looking-at flag-re))
              (forward-line)
            (setq key (match-string 1))
            ;; get description for key
            (setq start (match-end 0))
            (forward-line)
            (while (looking-at-p cont-re)
              (forward-line))
            (push
             (cons key
                   (replace-regexp-in-string
                    "^\\s-+\\|\t\\|\n$" ""
                    (buffer-substring start (1- (point)))))
             res))))
      (nreverse res))))

(provide 'nvp-help)
;;; nvp-help.el ends here
