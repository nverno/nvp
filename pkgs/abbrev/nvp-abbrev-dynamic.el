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


(eval-when-compile
  (defsubst nvp:local-abbrev-table ()
    "Return `local-abbrev-table', ensuring it is a list."
    (or (listp local-abbrev-table)
        (setq local-abbrev-table (list local-abbrev-table)))
    local-abbrev-table))

(defsubst nvp-abbrevd-p (table)
  "Check if TABLE is abbrevd."
  (intern-soft (symbol-name table) nvp-abbrevd-obarray))

(defsubst nvp-abbrevd--local-table (&optional mode-table)
  "Return value of local table or nil."
  (if (or mode-table (null (nvp:local-abbrev-table)))
      (let ((name (or nvp-local-abbrev-table (symbol-name major-mode))))
        (list (symbol-value (intern-soft (format "%s-abbrev-table" name)))))
    local-abbrev-table))


;;; Generics

(cl-defgeneric nvp-abbrevd-read (&optional arg)
  "Default method to read arguments for dynamic abbrev tables."
  ;; No prefix => new abbrev table from current buffer
  ;; C-u => new abbrev table from file
  ;; C-u C-u => new abbrevs from buffer
  (setq arg (or (if (consp arg) (car arg) arg) 0))
  (let ((file-p (= arg 4))
        (buffer-p (= arg 16)))
    (list (and file-p (read-file-name "File to abbrev: "))
          (unless file-p
            (if buffer-p
                (read-buffer "Buffer to abbrev: ")
              (current-buffer)))
          ;; (if (or (= arg 16)
          ;;         (and (or nvp-abbrevd-always-prompt
          ;;                  (> arg 16))
          ;;              (y-or-n-p "Add abbrevs to current table? ")))
          ;;     (nvp-abbrev--read-table nil nil 'local))
          )))


(cl-defgeneric nvp-abbrevd-make-args ()
  "Default method to produce arguments for `nvp-abbrevd--make-abbrevs'.
Called from buffer generated abbrevs."
  (list :objects (seq-uniq (nvp-parse-functions))
        :transformer #'nvp-abbrev--lisp-transformer))


(cl-defgeneric nvp-abbrevd-table-props (&key tables type)
  "Function to produce abbrev table properties.
TYPE is type of abbrevs (eg. \\='functions, \\='variables).
TABLES local abbrev table(s) for current mode.
The default uses props defined in TABLES."
  (let ((props (list :enable-function #'nvp-abbrev-expand-p
                     :type type)))
    (when tables
      (dolist (prop '(:regexp :enable-function :case-fixed))
        (--when-let (--some (abbrev-table-get it prop) tables)
          (plist-put props prop it))))
    props))


(defun nvp-abbrevd-make-table (prefix &optional hook &rest props)
  "Make new abbrevd table.
Its symbol is created from PREFIX and its function value is HOOK.
PROPS are passed to `make-abbrev-table'."
  (or (plist-get props :priority) (plist-put props :priority 1))
  (let ((sym (obarray-put nvp-abbrevd-obarray (symbol-name (cl-gensym prefix))))
        (table (make-abbrev-table props)))
    (set sym table)
    (fset sym hook)
    sym))

(defun nvp-abbrevd--active-tables (&optional table-list)
  "Return list of active abbrevd tables.
Search in `abbrev-table-name-list' or TABLE-LIST if non-nil."
  (let ((tables (or table-list abbrev-table-name-list))
        (res nil) cur)
    (while (setq cur (pop tables))
      (and (nvp-abbrevd-p cur)
           (push cur res)))
    res))

(defun nvp-abbrevd--mode-tables (&optional mode)
  "Return list of globally defined abbrevd tables for MODE."
  (or mode (setq mode (or nvp-mode-name major-mode)))
  (let (res)
    (obarray-map
     (lambda (table)
       (when (eq mode (abbrev-table-get (symbol-value table) :mode))
         (push table res)))
     nvp-abbrevd-obarray)
    res))

(defun nvp-abbrevd--make-table-props (&optional mode file type)
  "Return properties for abbrevd table."
  (or type (setq type 'functions))
  (or mode (setq mode (or nvp-mode-name major-mode)))
  (and file (setq file (abbreviate-file-name file)))
  (let ((props (nvp-abbrevd-table-props
                :tables (nvp-abbrevd--local-table 'mode-table)
                :type type)))
    (pcase-dolist (`(,prop . ,val) `((:mode . ,mode)
                                     (:file . ,file)
                                     (:type . ,type)))
      (when (and val (null (plist-get props prop)))
        (plist-put props prop val)))
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


;;; Commands

;;;###autoload
(defun nvp-abbrevd-kill-tables (&optional all)
  "Clear/remove local abbrevd tables.
With ALL (prefix), kill all abbrevd tables."
  (interactive "P")
  (let* ((locals (nvp:local-abbrev-table))
         (tables (nvp-abbrevd--active-tables
                  (if all
                      abbrev-table-name-list
                    (mapcar #'abbrev-table-name locals)))))
    (setq local-abbrev-table
          (--filter (not (memq (abbrev-table-name it) tables)) locals))
    (setq abbrev-table-name-list
          (--filter (not (memq it tables)) abbrev-table-name-list))
    (dolist (table tables)
      (obarray-remove nvp-abbrevd-obarray table))))


;;;###autoload
(defun nvp-abbrevd (&optional file buffer table)
  "Create dynamic abbrevs from FILE or BUFFER.
Add abbrevs to TABLE if non-nil."
  (interactive (nvp-abbrevd-read current-prefix-arg))
  (cl-assert (or file buffer))
  (let ((defs (with-current-buffer (or buffer (find-file-noselect file))
                (setq file (buffer-file-name))
                (apply #'nvp-abbrevd--make-abbrevs (nvp-abbrevd-make-args)))))
    (if (not defs)
        (when nvp-abbrev-verbose
          (message "No abbrevs found"))
      (let* ((mode (or nvp-mode-name major-mode))
             (props (nvp-abbrevd--make-table-props mode file)))
        (prog1 (apply #'nvp-abbrevd--populate-table
                      defs table (symbol-name mode) props)
          (when nvp-abbrev-verbose
            (message "Created %s abbrevs(%s)"
                     (length defs)
                     (if table
                         (format "in %S" (if (symbolp table) table "<abbrevd>"))
                       "++"))))))))


(provide 'nvp-abbrev-dynamic)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-abbrev-dynamic.el ends here
