;;; nvp-comment --- comment helpers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-03-05 18:30:13>
;; Created: 26 December 2018

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(autoload 'string-trim "subr-x")
(autoload 'string-trim-right "subr-x")
(autoload 'string-trim-left "subr-x")

(defun nvp-comment-string (str &optional padlen)
  "Wrap STR with modes starting and ending comment delimiters.
If PADLEN is non-nil, start with PADLEN comment starters."
  (let ((comment (if (or (derived-mode-p 'c-mode)
                         (memq major-mode '(python-mode))
                         (string= comment-end ""))
                     comment-start
                   (concat comment-start comment-start))))
    (when (and padlen (> padlen (length comment)))
      (setq comment (nvp-comment-start padlen comment)))
    (format "%s%s%s" comment str comment-end)))

(defun nvp-comment-start (length &optional start)
  "Create comment string of LENGTH starting with `comment-start' or START.
Accounts for multi-character comments by recycling the second character."
  (ignore-errors
    (let* ((comment (string-trim (or start comment-start)))
           (cont (if (> (length comment) 1) (aref comment 1) (aref comment 0))))
      (concat comment (make-string (max 0 (- length (length comment))) cont)))))

(defun nvp-comment-continued (length)
  "Make a comment continuation with LENGTH padding concated with `comment-end'."
  (if (and comment-end (not (string= "" comment-end)))
      (if (> (length comment-start) 1)
          (concat (make-string (1- length) ? ) (substring comment-start 1 2))
        (make-string length ? ))
    (nvp-comment-start length)))

(defun nvp-comment-end (&optional trim)
  "Return a TRIMmed version `comment-end' or \"\" if not defined."
  (if (bound-and-true-p comment-end)
      (and trim (string-trim comment-end))
    ""))

(provide 'nvp-comment)
;;; nvp-comment.el ends here
