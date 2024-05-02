;;; nvp-abbrev-dynamic.el --- dynamic abbrev tables -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Generics to generate abbrevs from buffer/file contents
;;
;; TODO(5/2/24):
;; - Edit dynamic abbrevs
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-abbrev)
(require 'nvp-parse)
(nvp:req 'nvp-abbrev 'subrs)
(nvp:auto "nvp-abbrev" nvp-abbrev--lisp-transformer)
(nvp:decls)


(define-abbrev-table 'nvp-abbrevd--table '()
  "Dynamic abbrev table.")

(defcustom nvp-abbrevd-always-prompt nil
  "Non-nil to always ask whether to append new abbrevs."
  :type 'boolean
  :safe 'booleanp
  :group 'abbrev)

(defvar nvp-abbrevd-verbose t
  "Non-nil to say stuff.")


(cl-defgeneric nvp-abbrevd-read (&optional arg)
  "Default method to read arguments for dynamic abbrev tables."
  (let ((arg (or (car arg) 0)))
    (list (if (>= arg 16)
              (read-file-name "File to abbrev: ")
            (current-buffer))
          (when nvp-abbrevd-table
            (if (or nvp-abbrevd-always-prompt
                    (>= arg 16))
                (y-or-n-p "Append to current dynamic table? ")
              (= arg 4))))))


(cl-defgeneric nvp-abbrevd-make-args ()
  "Default method to produce arguments for `nvp-abbrevd--make-abbrevs'.
Called from buffer generated abbrevs."
  (list :objects (seq-uniq (nvp-parse-functions))
        :transformer #'nvp-abbrev--lisp-transformer))


(cl-defgeneric nvp-abbrevd-table-props (&optional parent-table)
  "Function to produce abbrev table properties.
The default tries to use those defined in PARENT-TABLE."
  (let ((props (list :enable-function #'nvp-abbrev-expand-p)))
    (when parent-table
      (dolist (prop '(:regexp :enable-function))
        (and-let* ((val (abbrev-table-get parent-table prop)))
          (plist-put props prop val))))
    props))


(cl-defun nvp-abbrevd--populate-table
    (defs &optional append
          &key (parents (nvp-abbrev--local-table)) regexp enable-function)
  "Function to create and add dynamic abbrevs from current buffer.
If APPEND is non-nil, add abbrevs to current buffer-local dynamic table."
  (unless (consp parents) (setq parents (cons parents nil)))
  (when (and (not append)
             (bound-and-true-p nvp-abbrevd-table))
    (clear-abbrev-table nvp-abbrevd-table))
  (unless (abbrev-table-p nvp-abbrevd-table)
    (let ((table (make-abbrev-table)))
      (abbrev-table-put table :parents parents)
      (setq nvp-abbrevd-table table)))
  (abbrev-table-put nvp-abbrevd-table :regexp regexp)
  (abbrev-table-put nvp-abbrevd-table :enable-function enable-function)
  (dolist (def defs)
    (apply #'define-abbrev nvp-abbrevd-table def))
  (setq-local local-abbrev-table nvp-abbrevd-table))


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
                (let ((name (cond
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
(defun nvp-abbrevd (&optional buffer-or-file append)
  "Create dynamic abbrevs from BUFFER-OR-FILE.
If APPEND is non-nil, add abbrevs to current buffer-local dynamic table."
  (interactive (nvp-abbrevd-read current-prefix-arg))
  (let ((defs (with-current-buffer (if (buffer-live-p buffer-or-file)
                                       buffer-or-file
                                     (find-file-noselect buffer-or-file))
                (apply #'nvp-abbrevd--make-abbrevs (nvp-abbrevd-make-args)))))
    (if (not defs)
        (and nvp-abbrevd-verbose (message "No abbrevs found"))
      (let ((parent (nvp-abbrev--local-table)))
        (apply #'nvp-abbrevd--populate-table defs append
               :parents (list parent)
               (nvp-abbrevd-table-props parent))
        (when nvp-abbrevd-verbose
          (message "Created %s abbrevs(%s)"
                   (length defs) (if append "+" "-")))))))

;; -------------------------------------------------------------------
;;; Menu

(require 'transient)

(transient-define-infix nvp-abbrev-menu--splitters ()
  :class 'transient-lisp-variable
  :variable 'nvp-abbrev-splitters)

(transient-define-infix nvp-abbrev-menu--joiner ()
  :class 'transient-lisp-variable
  :variable 'nvp-abbrev-joiner)

(nvp:transient-toggle nvp-abbrev-menu nvp-abbrevd-verbose)

;;;###autoload(autoload 'nvp-abbrev-menu "nvp-abbrev-dynamic" nil t)
(transient-define-prefix nvp-abbrev-menu ()
  [["Dynamic"
    ("m" "Make from buffer/file" nvp-abbrevd)
    (":j" "Joiner" nvp-abbrev-menu--joiner)
    (":s" "Splitters" nvp-abbrev-menu--splitters)
    (":v" "Verbose" nvp-abbrev-menu--toggle-nvp-abbrevd-verbose)]
   ["Tables"
    ("s" "Save table" nvp-abbrev-write-abbrev-table)
    ("u" "Load unicode" nvp-abbrev-load-unicode)
    ("R" "Remove parent" nvp-abbrev-remove-parent)
    ("P" "Add parent" nvp-abbrev-add-parent)]
   ["Info"
    ("l" "List active" nvp-abbrev-list-abbrevs)
    ("p" "Table Props" nvp-abbrev-properties)]])

(provide 'nvp-abbrev-dynamic)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-abbrev-dynamic.el ends here
