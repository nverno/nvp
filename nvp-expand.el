;;; nvp-expand.el --- expansion functions -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'transient)
(nvp:decls :p (tiny) :v (tiny-beg) :f (tiny-expand))
(nvp:auto "nvp-edit-aux" 'nvp-list-wrap)

;;;###autoload(autoload 'nvp-expand-menu "nvp-expand" nil t)
(transient-define-prefix nvp-expand-menu ()
  "Expand"
  ["Expand region"
   ("a" "Awk it" awk-it)]
  ["Previous"
   ("." "Tiny" nvp-expand-tiny)
   ("r" "Range" nvp-expand-range)]
  ["Pattern"
   ("o" "Or" nvp-expand-or-pattern)])

;;;###autoload
(defun nvp-expand-range (&optional sep)
  "Expand range before point, eg. to make 0:10 by 1 could be 
either m:10, m0:10, m0:1:10 or m:1:10. So, a missing start assumes starts
from 0."
  (interactive
   (list (if current-prefix-arg (read-from-minibuffer "Separator: " ", ")
           " ")))
  (when (looking-back (nvp:concat "\\bm\\(-?[0-9.]*\\)\:"
                                  ;; either end of range or increment
                                  "\\(-?[0-9.]+\\)"
                                  ;; optional end of range
                                  "\\(?::\\(-?[0-9.]+\\)\\)?")
                      (line-beginning-position))
    (let ((start (match-string 1))
          (inc (match-string 2))
          (end (match-string 3)))
      (condition-case nil
          (let ((res (mapconcat 'number-to-string
                                (number-sequence
                                 (string-to-number start)
                                 (string-to-number (or end inc))
                                 (and end (string-to-number inc)))
                                (or sep " "))))
            (delete-region (match-beginning 0) (point))
            (insert res))
        (error)))))

;;;###autoload
(defun nvp-expand-tiny (&optional arg)
  "Expand region before point with `tiny-expand'. If prefix is non-nil,
wrap expanded items with quotes (default) or with double-prefix, prompt
for string to wrap.

Example: m,25+x?a%c => \"a\", \"b\", ..., \"z\"
"
  (interactive "P")
  (pcase arg
    ('(4)
     (tiny-expand)
     (funcall #'nvp-list-wrap tiny-beg (point) "\"" "\""))
    ('(16)
     (tiny-expand)
     (funcall-interactively #'nvp-list-wrap tiny-beg (point) nil nil nil 'prompt))
    (_ (tiny-expand))))

;;;###autoload
(defun nvp-expand-or-pattern ()
  "Expand blah.[a|b|c] on current line into blah.a\nblah.b\nblah.c"
  (interactive)
  (beginning-of-line)
  (when (and (re-search-forward "\\([^\[]*\\)\\[\\([^\]]+\\)\\]"
                                (line-end-position) 'noerror)
             (match-string 2))
    (replace-match
     (mapconcat (lambda (s) (concat (match-string 1) s))
                (save-match-data
                  (split-string (match-string 2) "|" 'omit " "))
                "\n")
     nil nil nil 0)))

(provide 'nvp-expand)
;;; nvp-expand.el ends here
