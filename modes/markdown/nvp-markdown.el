;;; nvp-markdown.el --- markdown helpers  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (defvar imenu-generic-expression))
(require 'nvp)
(nvp-package-define-root :snippets t)

;; -------------------------------------------------------------------
;;; Imenu

(defvar nvp-markdown-imenu-generic-expression
  '((nil   "^\\(.*\\)[\n]=+$"   1)   ;title 
    ("fn"  "^\\[\\^\\(.*\\)\\]" 1)   
    ("h1"  "^# \\(.*\\)$"       1)   
    ("h2"  "^## \\(.*\\)$"      1)   
    ("h2-" "^\\(.*\\)[\n]-+$"   1)   
    ("h3"  "^### \\(.*\\)$"     1)   
    ("h4"  "^#### \\(.*\\)$"    1)   
    ("h5"  "^##### \\(.*\\)$"   1)   
    ("h6"  "^###### \\(.*\\)$"  1))) 

;; Transform Elisp-style code references to Markdown-style.
(defun nvp-markdown-cleanup ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "`[^\n ]+\\('\\)" nil t)
    (replace-match "`" nil nil nil 1)))

(defun nvp-markdown-stack-block (beg end)
  (interactive "r")
  (let ((str (buffer-substring-no-properties beg end)))
    (delete-region beg end)
    (insert
     "<!-- language: lang-cl -->\n\n    "
     (mapconcat #'identity (split-string str "\n" t)
      "\n    "))))

(provide 'nvp-markdown)
;;; nvp-markdown.el ends here
