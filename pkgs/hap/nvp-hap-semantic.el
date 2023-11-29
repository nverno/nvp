;;; nvp-hap-semantic.el --- semantic help-at-point -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:auto "nvp-hap" nvp-hap-doc-buffer)

(nvp:decls :p (semantic) :v (semanticdb-current-table))

(defun nvp-hap-semantic-tag-at (point)
  (ignore-errors
    (car (reverse (oref (semantic-analyze-current-context point) prefix)))))

;;;###autoload
(defun nvp-hap-semantic-init ()
  (or (and (fboundp 'semantic-active-p) (semantic-active-p))
      (error "semantic not active")))

;;;###autoload
(defun nvp-hap-semantic (command &optional arg &rest _args)
  (cl-case command
    (init (nvp-hap-semantic-init))
    (thingatpt
     (-when-let (tag (nvp-hap-semantic-tag-at (point)))
       (and (semantic-tag-p tag) tag)))
    ;; (doc-string (semantic-documentation-for-tag arg))
    (doc-buffer
     (-when-let (tag (if (semantic-tag-p arg) arg
                       (nvp-hap-semantic-tag-at (point))))
       (let ((doc (or (semantic-documentation-for-tag tag)
                      ;; TODO: try includes
                      ;; (when-let* ((tab semanticdb-current-table)
                      ;;             (inc (semanticdb-includes-in-table tab))))
                      )))
         (with-current-buffer (nvp-hap-doc-buffer)
           (insert "Tag: ")
           (insert (semantic-format-tag-prototype tag))
           (insert "\nSnarfed Documentation:\n\n")
           (insert (if doc (princ doc) "  Documentation unavailable."))
           (list (current-buffer) nil nil)))))))

;;;###autoload
(defun nvp-hap-semantic-popup ()
  (interactive)
  (let ((nvp-help-at-point-functions '(nvp-hap-semantic)))
    (call-interactively #'nvp-help-at-point)))

(provide 'nvp-hap-semantic)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-hap-semantic.el ends here
