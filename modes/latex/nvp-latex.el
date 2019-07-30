;;; nvp-latex.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'latex nil t)
(require 'tex-site nil t)
(declare-function TeX-revert-document-buffer "")
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

(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
(declare-function info-lookup-add-help "info-look")
(with-eval-after-load 'info-look
  (info-lookup-add-help
   :mode 'LaTeX-mode
   :parse-rule "\\\\?[a-zA-Z]+\\|\\\\[^a-zA-Z]"
   :doc-spec '(("(latex2e)Concept Index")
               ("(latex2e)Command Index"))))

(provide 'nvp-latex)
;;; nvp-latex.el ends here
