;;; sh-doc.el --- documentation for sh functions -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/shell-tools
;; Last modified: <2019-03-15 18:24:52>
;; Created: 17 August 2018

;;; Commentary:

;; TODO:
;; - better check for doc block
;; - extend newline-dwim with doc-newline-dwim
;; - parse doc comments
;; - eldoc support

;;; Code:
(eval-when-compile
  (require 'cl-lib))
(require 'sh-script)

(eval-and-compile
  (defvar sh-doc-types '("brief" "param" "return" "note" "usage" "see")))
(defvar sh-doc-offset-column 16)

(defsubst sh-doc-active-p ()
  (save-excursion
    (or (nth 4 (syntax-ppss))
        (and (bolp) (looking-at-p "#")))))

(defun sh-doc-insert (type)
  (interactive (list (completing-read "doc: " sh-doc-types)))
  (and (bolp) (insert "# "))
  (insert "@" type)
  (indent-to-column sh-doc-offset-column))

;;;###autoload
(defun sh-doc-indent-dwim ()
  (interactive)
  (when (and (sh-doc-active-p)
             ;; indent documentation to offset column if necessary
             (or (not (eq (move-to-column sh-doc-offset-column)
                          sh-doc-offset-column))
                 (not (or (eolp)
                          (looking-at-p "\\s-*$")))))
    (beginning-of-line)
    (if (not (looking-at-p (eval-when-compile
                             (concat "#\\s-*@" (regexp-opt sh-doc-types)))))
        (progn (end-of-line)            ;no info keyword, go to end of line
               (call-interactively 'sh-doc-insert))
      (forward-word)
      (delete-horizontal-space)
      (indent-to-column sh-doc-offset-column))))

;;;###autoload
(defun sh-doc-newline-dwim ()
  (interactive)
  (end-of-line)
  (if (sh-doc-active-p)
      (progn
        (newline)
        (call-interactively 'sh-doc-insert))
    (newline)
    (insert "# ")))

(provide 'sh-doc)
;;; sh-doc.el ends here
