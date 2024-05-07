;;; nvp-abbrev-dynamic.el --- dynamic abbrev tables -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Generics to generate abbrevs from buffer/file contents
;;
;; TODO(5/2/24):
;; - Edit dynamic abbrevs
;; -
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(eval-and-compile (require 'nvp-abbrev))
(require 'nvp-parse)
(nvp:decls)


(defcustom nvp-abbrevd-always-prompt nil
  "Non-nil to always ask whether to append new abbrevs."
  :type 'boolean
  :safe 'booleanp
  :group 'abbrev)


(defun nvp-abbrevd-make-table (prefix &optional hook &rest props)
  "Make new abbrevd table.
Its symbol is created from PREFIX and its function value is HOOK.
PROPS are passed to `make-abbrev-table'."
  (let ((sym (obarray-put nvp-abbrevd-obarray (symbol-name (cl-gensym prefix))))
        (table (make-abbrev-table props)))
    (set sym table)
    (fset sym hook)
    (while (consp props)
      (unless (cdr props) (error "Missing value for property %S" (car props)))
      (abbrev-table-put table (pop props) (pop props)))
    sym))

(defsubst nvp-abbrevd-p (table)
  "Check if TABLE is abbrevd."
  (intern-soft (symbol-name table) nvp-abbrevd-obarray))

(defsubst nvp-abbrevd--local-table ()
  "Return value of local table or nil."
  (or local-abbrev-table
      (let ((name (or nvp-local-abbrev-table (symbol-name major-mode))))
        (symbol-value (intern-soft (format "%s-abbrev-table" name))))))

(defun nvp-abbrevd--active-tables (&optional table-list)
  "Return list of active abbrevd tables.
Search in `abbrev-table-name-list' or TABLE-LIST if non-nil."
  (let ((tables (or table-list abbrev-table-name-list))
        (res nil) cur)
    (while (setq cur (pop tables))
      (and (nvp-abbrevd-p cur)
           (push res cur)))
    res))


(cl-defgeneric nvp-abbrevd-read (&optional arg)
  "Default method to read arguments for dynamic abbrev tables."
  ;; No prefix => new abbrev table from current buffer
  ;; C-u => new abbrev table from file
  ;; C-u C-u => add abbrevs from current buffer to another table
  ;; C-u C-u C-u... => prompt for both options
  (let ((arg (or (car arg) 0)))
    (list (if (or (= arg 4)
                  (and (> arg 16)
                       (y-or-n-p "From file? ")))
              (read-file-name "File to abbrev: ")
            (current-buffer))
          (if (or (= arg 16)
                  (and (or nvp-abbrevd-always-prompt
                           (> arg 16))
                       (y-or-n-p "Add abbrevs to current table? ")))
              (nvp-abbrev--read-table nil nil 'local)))))


(cl-defgeneric nvp-abbrevd-make-args ()
  "Default method to produce arguments for `nvp-abbrevd--make-abbrevs'.
Called from buffer generated abbrevs."
  (list :objects (seq-uniq (nvp-parse-functions))
        :transformer #'nvp-abbrev--lisp-transformer))


(cl-defgeneric nvp-abbrevd-table-props (&optional parent-tables)
  "Function to produce abbrev table properties.
The default tries to use those defined in PARENT-TABLES."
  (let ((props (list :enable-function #'nvp-abbrev-expand-p
                     :priority 1)))
    (when parent-tables
      (setq parent-tables (--map (if (symbolp it) (symbol-value it) it)
                                 (if (listp parent-tables)
                                     parent-tables
                                   (list parent-tables))))
      (dolist (prop '(:regexp :enable-function))
        (--when-let (--some (abbrev-table-get it prop) parent-tables)
          (plist-put props prop it))))
    props))


(defun nvp-abbrevd--populate-table (defs &optional table prefix &rest props)
  "Add abbrev DEFS to TABLE or create a new abbrevd table."
  (cl-assert (or table prefix) t "One is required")
  (and table (setq table (nvp-abbrev--table-value table)))
  (unless table
    (let ((sym (apply #'nvp-abbrevd-make-table prefix nil props)))
      (setq table (symbol-value sym))
      (unless (memq sym abbrev-table-name-list)
        (push sym abbrev-table-name-list))
      (when nvp-abbrev-verbose
        (message "New abbrevd table: '%S'" sym)))
    (nvp-abbrev--add-local table))
  (dolist (def defs)
    (apply #'define-abbrev table def))
  table)


;; Create abbrevs from obarray/list/symbol/string
;; MIN-LENGTH determines the cutoff length for objects to consider for abbrevs
;; PREDICATE is a function called with one arg, the candidate, returning non-nil
;; if the candidate should be considered as an abbrev.
;; TRANSFORMER is a function called with one arg, the candidate, returning
;; the abbreviated value to use for expansion
(cl-defun nvp-abbrevd--make-abbrevs
    (&key (objects obarray)
          (min-length 4)
          predicate
          (transformer #'nvp-abbrev--lisp-transformer))
  "Function to convert objects into their abbreviated forms."
  (let (res)
    (cl-flet ((make-abbrev (name)
                (let ((name
                       (cond
                        ((stringp name) name)
                        ((symbolp name) (symbol-name name))
                        (t nil))))
                  (when (and name
                             (< min-length (length name))
                             (if predicate (funcall predicate name) t))
                    (condition-case nil
                        (push (cons (funcall transformer name)
                                    (cons name nil))
                              res)
                      (error nil))))))
      (cond ((or (symbolp objects)
                 (stringp objects))
             (make-abbrev objects))
            ((arrayp objects)
             (mapatoms (function make-abbrev) objects))
            ((listp objects)
             (mapc (function make-abbrev) objects))))
    res))


;;;###autoload
(defun nvp-abbrevd (&optional buffer-or-file table)
  "Create dynamic abbrevs from BUFFER-OR-FILE.
Add abbrevs to TABLE if non-nil."
  (interactive (nvp-abbrevd-read current-prefix-arg))
  (let ((defs (with-current-buffer (if (buffer-live-p buffer-or-file)
                                       buffer-or-file
                                     (find-file-noselect buffer-or-file))
                (apply #'nvp-abbrevd--make-abbrevs
                       (nvp-abbrevd-make-args)))))
    (if (not defs)
        (when nvp-abbrev-verbose
          (message "No abbrevs found"))
      (let* ((parents (nvp-abbrevd--local-table))
             (props (nvp-abbrevd-table-props parents))
             (prefix (symbol-name (or nvp-mode-name major-mode))))

        (apply #'nvp-abbrevd--populate-table
               defs table prefix props)

        (when nvp-abbrev-verbose
          (message "Created %s abbrevs(%s)"
                   (length defs)
                   (if table
                       (format "in %S" (if (symbolp table) table "<abbrevd>"))
                     "++")))))))


(provide 'nvp-abbrev-dynamic)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-abbrev-dynamic.el ends here
