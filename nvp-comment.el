;;; nvp-comment.el --- comment helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp-autoload "subr-x" string-trim 'string-trim-right 'string-trim-left)

(defun nvp-comment-string (str &optional padlen)
  "Wrap STR with modes starting and ending comment delimiters.
If PADLEN is non-nil, start with PADLEN comment starters."
  (let* ((ctrim (string-trim-right comment-start))
         (comment (if (or (derived-mode-p 'c-mode)
                         (memq major-mode '(python-mode))
                         (string= comment-end ""))
                      ctrim
                    (concat ctrim ctrim))))
    (when (and padlen (> padlen (length comment)))
      (setq comment (nvp-comment-start padlen comment)))
    (format "%s %s%s" comment str comment-end)))

(defun nvp-comment-start (length &optional start)
  "Create comment string of LENGTH starting with `comment-start' or START.
Accounts for multi-character comments by recycling the second character."
  (let* ((comment (or start (string-trim-right comment-start)))
         (cont (if (> (length comment) 1) (aref comment 1) (aref comment 0))))
    (concat comment (make-string (max 0 (- length (length comment))) cont))))

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
