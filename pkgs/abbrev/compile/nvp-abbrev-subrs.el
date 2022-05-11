;;; nvp-abbrev-subrs.el --- shared abbrev utils -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; shared abbrev subrs required by multiple files
;;
;;; Code:
(require 'nvp-macro)
(require 'abbrev)
(require 'nvp)

;; get the value of the local abbrev table
(defsubst nvp-abbrev--local-table ()
  (or local-abbrev-table
      (symbol-value (intern (format "%s-abbrev-table" (symbol-name major-mode))))))

;; return list of currently loaded and non-empty abbrev tables
(defsubst nvp-abbrev--nonempty (&optional tables)
  (cl-remove-if
   (lambda (table)
     (abbrev-table-empty-p (symbol-value table)))
   (or tables abbrev-table-name-list)))

;; list of all table parents, recursively
(defsubst nvp-abbrev--all-parents (table &optional names)
  (unless (abbrev-table-p table) (setq table (symbol-value table)))
  (let ((parents (abbrev-table-get table :parents))
        res)
    (while parents
      (setq res (append res parents))
      (setq parents (car (mapcar (lambda (tab) (abbrev-table-get tab :parents)) parents))))
    (if names (mapcar #'abbrev-table-name res)
      res)))

;; transform by splitting on '-', eg.
;; 'nvp-abbrev--lisp-transformer' => 'na:lt' abbrev
(defsubst nvp-abbrev--lisp-transformer (str &optional joiner splitter)
  (nvp:defq splitter "-" joiner ":")
  (mapconcat (lambda (s)
               (if (string-empty-p s) joiner
                 (substring s 0 1)))
             (split-string str "-") ""))

(defsubst nvp-abbrev--read-table (prompt choices)
  (nvp-completing-read prompt choices nil t nil 'nvp-abbrev--read-history))

;; Grab preceding NCHARS to match against in abbrev table.
(defsubst nvp-abbrev--grab-prev (nchars)
  (save-excursion
    (let ((end (point))
          (_ (skip-chars-backward nvp-abbrev-prefix-chars (- (point) nchars)))
          (start (point)))
      (buffer-substring-no-properties start end))))

(provide 'nvp-abbrev-subrs)
;;; nvp-abbrev-subrs.el ends here
