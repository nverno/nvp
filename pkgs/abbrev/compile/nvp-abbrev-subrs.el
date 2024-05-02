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

;; Note: The dynamic table doesnt have an indirect evaled symbol like normal
;; abbrev tables
(defsubst nvp:table-value (table)
  (if (symbolp table) (symbol-value table) table))

(defsubst nvp:table-name (table)
  (if (symbolp table) (symbol-name table) "Dynamic"))

;; get the value of the local abbrev table
(defsubst nvp-abbrev--local-table ()
  (or local-abbrev-table
      (symbol-value (intern (format "%s-abbrev-table" (symbol-name major-mode))))))

;; return list of currently loaded and non-empty abbrev tables
(defsubst nvp-abbrev--nonempty (&optional tables)
  (cl-remove-if
   (lambda (table) (abbrev-table-empty-p (nvp:table-value table)))
   (or tables (if (bound-and-true-p nvp-abbrevd-table)
                  abbrev-table-name-list
                (delq 'nvp-abbrevd--table abbrev-table-name-list)))))

;;; XXX(5/2/24): check for dependency loop? hasnt ever happened, but maybe...
;; list of all table parents, recursively
(defsubst nvp-abbrev--all-parents (table &optional names)
  (unless (abbrev-table-p table) (setq table (symbol-value table)))
  (let ((parents (abbrev-table-get table :parents)) res)
    (while parents
      (setq res (append res parents))
      (setq parents (car (mapcar (lambda (tab) (abbrev-table-get tab :parents)) parents))))
    (if names (mapcar #'abbrev-table-name res)
      res)))

;; Grab preceding NCHARS to match against in abbrev table.
(defsubst nvp-abbrev--grab-prev (nchars)
  (save-excursion
    (let ((end (point))
          (_ (skip-chars-backward nvp-abbrev-prefix-chars (- (point) nchars)))
          (start (point)))
      (buffer-substring-no-properties start end))))

(provide 'nvp-abbrev-subrs)
;;; nvp-abbrev-subrs.el ends here
