;;; nvp-abbrev-util.el --- shared abbrev utils -*- lexical-binding: t; -*-

;;; Commentary:

;; shared abbrev utils required by multiple files

;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'abbrev)

;; get the value of the local abbrev table
(defun nvp-abbrev--local-table ()
  (or local-abbrev-table
      (symbol-value (intern (format "%s-abbrev-table" (symbol-name major-mode))))))

;; return list of currently loaded and non-empty abbrev tables
(defsubst nvp-abbrev--nonempty (&optional tables)
  (cl-remove-if
   (lambda (table)
     (abbrev-table-empty-p (symbol-value table)))
   (or tables abbrev-table-name-list)))

;; get list of all table properties, converting parent tables into symbols
(defun nvp-abbrev--get-plist (table)
  (unless (abbrev-table-p table) (setq table (symbol-value table)))
  (when-let* ((sym (obarray-get table ""))
              (props (symbol-plist sym)))
    (cl-loop for (k v) on props by #'cddr
       if (eq :parents k)
       collect (list k (nvp-abbrev--all-parents table 'names))
       else
       collect (list k v))))

;; list of all table parents, recursively
(defun nvp-abbrev--all-parents (table &optional names)
  (unless (abbrev-table-p table) (setq table (symbol-value table)))
  (let ((parents (abbrev-table-get table :parents))
        res)
    (while parents
      (setq res (append res parents))
      (setq parents (car (mapcar (lambda (tab) (abbrev-table-get tab :parents)) parents))))
    (if names (mapcar #'abbrev-table-name res)
      res)))

;; list all active, nonempty tables:
;; - dynamic table, local table, all parents, global table
(defun nvp-abbrev--active-tables (&optional allow-empty)
  (let ((tabs
         (append (if (null local-abbrev-table) ()
                   (cons local-abbrev-table
                         (nvp-abbrev--all-parents local-abbrev-table)))
                 (list global-abbrev-table))))
    (when (and nvp-abbrev-dynamic-table
               (abbrev-table-p nvp-abbrev-dynamic-table))
      (setq tabs (cons nvp-abbrev-dynamic-table tabs)))
    (setq tabs (delete-dups (mapcar #'abbrev-table-name tabs)))
    (if allow-empty tabs
      (nvp-abbrev--nonempty tabs))))

;; transform by splitting on '-', eg.
;; 'nvp-abbrev--lisp-transformer' => 'na:lt' abbrev
(defun nvp-abbrev--lisp-transformer (str &optional joiner splitter)
  (or splitter (setq splitter "-"))
  (or joiner (setq joiner ":"))
  (mapconcat (lambda (s)
               (if (string-empty-p s) joiner
                 (substring s 0 1)))
             (split-string str "-") ""))

(provide 'nvp-abbrev-util)
;;; nvp-abbrev-util.el ends here
