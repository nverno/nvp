;;; nvp-hap-semantic.el --- semantic help-at-point -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'nvp-hap)
(require 'semantic/analyze)
(nvp:decls)
(declare-function semanticdb-includes-in-table "semantic/db-ref")
(defvar semanticdb-current-table)

(defsubst nvp-semantic-tag-at (point)
  (ignore-errors
    (car (reverse (oref (semantic-analyze-current-context point) prefix)))))

;;;###autoload
(defun nvp-hap-semantic (command &optional arg &rest _args)
  (cl-case command
    (thingatpt
     (-when-let (tag (nvp-semantic-tag-at (point)))
       (and (semantic-tag-p tag) tag)))
    ;; (doc-string (semantic-documentation-for-tag arg))
    (doc-buffer
     (-when-let (tag (if (semantic-tag-p arg) arg
                       (nvp-semantic-tag-at (point))))
       (let ((doc (or (semantic-documentation-for-tag tag)
                      ;; TODO: try includes
                      ;; (when-let* ((tab semanticdb-current-table)
                      ;;             (inc (semanticdb-includes-in-table tab))))
                      )))
         (with-current-buffer (nvp-hap-doc-buffer)
           (insert "Tag: ")
           (insert (semantic-format-tag-prototype tag))
           (insert "\n\nSnarfed Documentation:\n\n")
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
