;;; nvp-sml.el --- smluts -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls :p sml)


(with-eval-after-load 'nvp-repl
  (require 'nvp-sml-repl))

(nvp:defmethod nvp-newline-dwim-comment (syntax arg)
  :modes (sml-mode sml-ts-mode)
  (nvp-newline-dwim--comment syntax arg " * "))

(defvar nvp-sml-source-repo "https://github.com/smlnj/smlnj")
(defvar nvp-sml-source-dir (expand-file-name "smlnj" (getenv "DEVEL")))

(defun nvp-sml-tag-source (source-repo source-dir &optional tag-file no-continue)
  "Clone and/or update source and tag it."
  (interactive (list nvp-sml-source-repo nvp-sml-source-dir "TAGS"))
  (nvp-tag-repo source-repo source-dir tag-file no-continue))

(defun nvp-sml-lookup-basis (kind)
  "Lookup KIND in Standard ML Basis Library online documentation."
  (interactive
   (list
    (completing-read "Type: "
      '("value" "type" "structure" "signature" "functor" "exception"))))
  (call-interactively (intern (format "sml-basis-lookup-%s" kind))))

(provide 'nvp-sml)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-sml.el ends here
