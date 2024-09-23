;;; nvp-sml.el --- smluts -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls :p sml)


(with-eval-after-load 'nvp-repl
  (require 'nvp-sml-repl))

(cl-defmethod nvp-newline-dwim-comment
  (syntax arg &context (major-mode sml-mode))
  (nvp-newline-dwim--comment syntax arg))

(defvar nvp-sml-source-repo "https://smlnj-gforge.cs.uchicago.edu/svn")
(defvar nvp-sml-source-dir (expand-file-name "sml" (getenv "DEVEL")))

(defun nvp-sml-tag-source (source-repo source-dir &optional tag-file no-continue)
  "Clone and/or update source and tag it."
  (interactive (list nvp-sml-source-repo nvp-sml-source-dir "TAGS"))
  (nvp-tag-repo source-repo source-dir tag-file no-continue))

(provide 'nvp-sml)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-sml.el ends here
