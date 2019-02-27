;;; nvp-latex.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-27 13:19:46>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/md-tools
;; Created: 31 January 2019

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(require 'nvp)
(require 'latex nil t)
(require 'tex-site nil t)
(autoload 'TeX-run-BibTeX "tex-buf")
(autoload 'TeX-command "tex-buf")

(defun nvp-latex-electric-| ()
  (interactive)
  (insert "\\| \\| ") (backward-char 3))

(defun nvp-latex-run-bibtex ()
  (interactive)
  (let ((file (file-name-nondirectory
               (file-name-sans-extension
                (buffer-file-name)))))
    (TeX-run-BibTeX
     "BibTeX"
     (format "bibtex %s" file)
     file)))

(defun nvp-latex-pdflatex-compile ()
  "Compile .tex file and show .pdf file."
  (interactive)
  (save-buffer)
  (if (file-exists-p (expand-file-name "Makefile"))
      ;; (compile "make")
      (TeX-command "make" 'TeX-master-file -1)
    (TeX-command "LaTeX" 'TeX-master-file -1)))

(provide 'nvp-latex)
;;; nvp-latex.el ends here
