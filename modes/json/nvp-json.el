;;; nvp-json.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'json-mode)

(defconst nvp-json-syntax-table
  (let ((tab (copy-syntax-table json-mode-syntax-table)))
    (modify-syntax-entry ?. "_" tab)
    tab))

;; expand x.y => "x" : "y"
(defun nvp-json-expand-dot (bnds)
  (interactive
   (list (with-syntax-table nvp-json-syntax-table
           (bounds-of-thing-at-point 'symbol))))
  (and bnds
       (cl-destructuring-bind (a b)
           (split-string (buffer-substring-no-properties (car bnds) (cdr bnds))
                         "[.]" 'omit " ")
         (delete-region (car bnds) (cdr bnds))
         (insert (format "\"%s\": \"%s\"" a b)))))


(provide 'nvp-json)
;;; nvp-json.el ends here
