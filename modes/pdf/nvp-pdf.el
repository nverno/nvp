;;; nvp-pdf.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'pdf-tools)
(nvp:decls :v (pdf-info-epdfinfo-program
               pdf-tools-auto-mode-alist-entry
               pdf-annot-default-annotation-properties)
           :f (pdf-annot-add-text-annotation
               pdf-history-forward pdf-history-backward))

;; build if necessary
(defun nvp-pdf-setup ()
  (unless (file-exists-p pdf-info-epdfinfo-program)
    (pdf-tools-install t t t)
    ;; (call-interactively 'nvp-pdf-install-epdfinfo))
    ;; add enty that `pdf-tools-install' would add
    (add-to-list 'auto-mode-alist pdf-tools-auto-mode-alist-entry)))

;; -------------------------------------------------------------------
;;; Annotations

;; Read an annotation label when attaching
(defun nvp-pdf-add-text-annotation (label)
  (interactive 
   (list
    (read-string (format "Label (%s): " user-full-name) nil nil user-full-name)))
  (let ((pdf-annot-default-annotation-properties
         (cons `(t (label . ,label))
               (cdr pdf-annot-default-annotation-properties))))
    (call-interactively #'pdf-annot-add-text-annotation)))

(provide 'nvp-pdf)
;;; nvp-pdf.el ends here
