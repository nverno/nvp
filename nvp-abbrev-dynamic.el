;;; nvp-abbrev-dynamic.el --- Generate dynamic abbrev tables -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-22 16:22:14>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created:  7 February 2019

;;; Commentary:

;; Generics to generate abbrevs from buffer/file contents
;; TODO: 
;; - refactor
;; - generic to make abbrevs
;; - generic to create table

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(require 'nvp)
(require 'nvp-parse)
(require 'nvp-abbrev-util)

;;; FIXME:
(cl-defgeneric nvp-abbrev-dynamic--read (&optional arg)
  "Default method to read arguments for dynamic abbrev tables."
  (list (and arg (read-file-name "File to abbrev: "))
        (y-or-n-p "Append to current dynamic table? ")))

(cl-defun nvp-abbrev-dynamic (&optional buffer-or-file append)
  "Create dynamic abbrevs from BUFFER-OR-FILE.
If APPEND is non-nil, add abbrevs to current buffer-local dynamic table."
  (interactive (nvp-abbrev-dynamic--read current-prefix-arg))
  (let ((buff (if (buffer-live-p buffer-or-file)
                  buffer-or-file
                (find-file-noselect buffer-or-file))))
    (set-buffer buff)
    (nvp-abbrev-dynamic-populate-table append)))

(cl-defun nvp-abbrev-dynamic-populate-table
    (&optional append &keys (parents (nvp-abbrev--local-table)) regexp
       enable-function)
  "Function to create and add dynamic abbrevs from current buffer.
If APPEND is non-nil, add abbrevs to current buffer-local dynamic table."
  (unless (consp parents) (setq parents (cons parents nil)))
  (when-let ((fns (nvp-parse-function-names)))
    (when (and (not append) (bound-and-true-p nvp-abbrev-dynamic-table))
      (clear-abbrev-table nvp-abbrev-dynamic-table))
    (unless (abbrev-table-p nvp-abbrev-dynamic-table)
      (define-abbrev-table 'nvp-abbrev-dynamic-table nil
        :parents parents
        :regexp regexp
        :enable-function enable-function))
    (setq-local local-abbrev-table nvp-abbrev-dynamic-table)
    (pcase-dolist (`(,abbr ,exp) (nvp-abbrev--make-abbrevs :objects fns))
      (define-abbrev nvp-abbrev-dynamic-table abbr exp))))

;; Create abbrevs from obarray/list/symbol/string
;; MIN-LENGTH determines the cutoff length for objects to consider for abbrevs
;; PREDICATE is a function called with one arg, the candidate, returning non-nil
;; if the candidate should be considered as an abbrev.
;; TRANSFORMER is a function called with one arg, the candidate, returning
;; the abbreviated value to use for expansion
(cl-defun nvp-abbrev--make-abbrevs (&key
                                      (objects obarray)
                                      (min-length 4)
                                      predicate
                                      (transformer #'nvp-abbrev--lisp-transformer))
  "Function to convert objects into their abbreviated forms."
  (let (res)
    (cl-flet ((make-abbrev (name)
                (let ((name (cond
                              ((stringp name) name)
                              ((symbolp name) (symbol-name name))
                              (t nil))))
                  (when (and name
                             (< min-length (length name))
                             (if predicate (funcall predicate name) t))
                    (condition-case nil
                        (push (cons (funcall transformer name) (cons name nil)) res)
                      (error nil))))))
      (cond
        ((or (symbolp objects)
             (stringp objects))
         (make-abbrev objects))
        ((arrayp objects)
         (mapatoms (function make-abbrev) objects))
        ((listp objects)
         (mapc (function make-abbrev) objects))))
    res))

(provide 'nvp-abbrev-dynamic)
;;; nvp-abbrev-dynamic.el ends here

;; Local Variables:
;; lisp-indent-function: common-lisp-indent-function
;; End:
