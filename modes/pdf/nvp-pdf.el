;;; nvp-pdf.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'pdf-tools nil t)
(nvp:decls :p (pdf))

(defun nvp-pdf-add-text-annotation (label)
  "Read an annotation LABEL when attaching."
  (interactive 
   (list
    (read-string (format "Label (%s): " user-full-name) nil nil user-full-name)))
  (let ((pdf-annot-default-annotation-properties
         (cons `(t (label . ,label))
               (cdr pdf-annot-default-annotation-properties))))
    (call-interactively #'pdf-annot-add-text-annotation)))

(provide 'nvp-pdf)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-pdf.el ends here
