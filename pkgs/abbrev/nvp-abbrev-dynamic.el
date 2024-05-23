;;; nvp-abbrev-dynamic.el --- dynamic abbrev tables -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Generics to generate abbrevs from buffer/file contents
;;
;; TODO(5/7/24):
;; - Sort local abbrev tables by priority
;; - Update abbrevd table props in edit
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(eval-and-compile
  (require 'nvp-abbrev))
(nvp:decls)
(defvar nvp-abbrev-completion-need-refresh)


(eval-when-compile
  (defsubst nvp:local-abbrev-table ()
    "Return `local-abbrev-table', ensuring it is a list."
    (or (listp local-abbrev-table)
        (setq local-abbrev-table (list local-abbrev-table)))
    local-abbrev-table)

  (defsubst nvp:abbrevd-read-tables (prompt tables)
    "Call `completing-read-multiple' with PROMPT and TABLES.
Return list of symbols interned from `nvp-abbrevd-obarray'."
    (--map (intern-soft it nvp-abbrevd-obarray)
           (completing-read-multiple prompt tables nil t))))

(defsubst nvp-abbrevd-get-table (table)
  "Check if TABLE is abbrevd."
  (intern-soft (symbol-name table) nvp-abbrevd-obarray))

(defsubst nvp-abbrevd--local-table (&optional mode use-default)
  "Return value of local table or nil."
  (if (or mode use-default (null (nvp:local-abbrev-table)))
      (let ((name (or (and mode (symbol-name mode))
                      nvp-local-abbrev-table
                      (symbol-name major-mode))))
        (list (symbol-value (intern-soft (format "%s-abbrev-table" name)))))
    local-abbrev-table))


(defun nvp-abbrevd-read (&optional arg)
  "Interactively read abbrevd args depending on ARG.
With more than two prefixes, prompt for mode to use for abbrevds."
  (setq arg (or (if (consp arg) (car arg) arg) 0))
  (let* ((default-mode (or nvp-mode-name major-mode))
         (mode (if (> arg 16)
                   (intern (nvp-read-mode default-mode))
                 default-mode)))
    (cons mode (nvp-abbrevd-read-args mode arg))))

(defun nvp-abbrevd-read-defaults (&optional arg reader-fn)
  "Interactively read default abbrevd args depending on ARG.
If ARG is nil create new abbrevd table from current buffer.
With \\[universal-argument] prompt for file or package to abbrev.
With \\[universal-argument] \\[universal-argument] prompt for buffer to abbrev.
If READER-FN is non-nil, it is called instead of `read-file-name'.
READER-FN should return a plist that gets passed on to `nvp-abbrevd-make-args'."
  (let ((file-or-package-p (= arg 4))
        (buffer-p (= arg 16)))
    (nconc (and file-or-package-p
                (if reader-fn
                    (funcall reader-fn)
                  (list :file (read-file-name "File to abbrev: "))))
           (unless file-or-package-p
             (list :buffer (if buffer-p
                               (get-buffer (read-buffer "Buffer to abbrev: "))
                             (current-buffer))))
           (list :type 'functions))))


(cl-defgeneric nvp-abbrevd-read-args (&optional _mode arg &rest _args)
  "Read arguments for abbrevd tables."
  (nvp-abbrevd-read-defaults arg))

(cl-defmethod nvp-abbrevd-read-args
  ((_mode (eql emacs-lisp-mode)) &optional arg &rest _args)
  "Read library with single prefix in elisp."
  (nvp-abbrevd-read-defaults
   arg (lambda () (list :library (concat (read-library-name) ".el")))))


(cl-defgeneric nvp-abbrevd-make-args (&optional mode &rest args)
  "Default method to produce arguments for `nvp-abbrevd--make-abbrevs'.
Called from buffer generated abbrevs."
  (require 'nvp-parse)
  (list :objects (seq-uniq (apply #'nvp-parse-functions mode args))
        :transformer #'nvp-abbrev-from-splitters))


(cl-defgeneric nvp-abbrevd-table-props
    (&optional mode &key tables type file buffer priority &allow-other-keys)
  "Function to produce abbrev table properties.
TYPE is type of abbrevs (eg. \\='functions, \\='variables).
TABLES local abbrev table(s) for current mode.
The default uses props defined in TABLES."
  (let* ((file (or file (and buffer (buffer-file-name buffer))))
         (props (nconc (list :enable-function #'nvp-abbrev-expand-p
                             :type (or type 'functions)
                             :priority (or priority 1)
                             :mode mode)
                       (and file (list :file (abbreviate-file-name file))))))
    (when tables
      (dolist (prop '(:regexp :enable-function :case-fixed))
        (--when-let (--some (abbrev-table-get it prop) tables)
          (plist-put props prop it))))
    props))


(defun nvp-abbrevd--make-table (prefix &optional hook &rest props)
  "Make new abbrevd table.
Its symbol is created from PREFIX and its function value is HOOK.
PROPS are passed to `make-abbrev-table'."
  (plist-put props :nvp-abbrevd t)
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
      (and (nvp-abbrevd-get-table cur)
           (push cur res)))
    res))

(defun nvp-abbrevd--mode-tables (&optional modes)
  "Return list of globally defined abbrevd tables for MODES."
  (or modes (setq modes (list (or nvp-mode-name major-mode))))
  (let (res)
    (obarray-map
     (lambda (table)
       (when (memq (abbrev-table-get (symbol-value table) :mode) modes)
         (push table res)))
     nvp-abbrevd-obarray)
    res))


(defun nvp-abbrevd--populate-table (defs &optional table prefix &rest props)
  "Add abbrev DEFS to TABLE or create a new abbrevd table."
  (cl-assert (or table prefix) t "One is required")
  (and table (setq table (nvp-abbrev--table-value table)))
  (unless table
    (let ((sym (apply #'nvp-abbrevd--make-table prefix nil props)))
      (setq table (symbol-value sym))
      (unless (memq sym abbrev-table-name-list)
        (push sym abbrev-table-name-list))
      (nvp-abbrev-msg "New abbrevd table: '%S'" sym))
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
          (min-abbrev-len 2)
          (transformer #'nvp-abbrev-from-splitters)
          predicate
          &allow-other-keys)
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
                        (let ((abbr (funcall transformer name)))
                          (and (>= (length abbr) min-abbrev-len)
                               (push (list abbr name) res)))
                      (error nil))))))
      (cond ((or (symbolp objects)
                 (stringp objects))
             (make-abbrev objects))
            ((arrayp objects)
             (mapatoms (function make-abbrev) objects))
            ((listp objects)
             (mapc (function make-abbrev) objects))))
    res))


;; -------------------------------------------------------------------
;;; Edit

;;;###autoload
(defun nvp-abbrevd-edit-abbrevs (&optional modes all table-list)
  "Edit abbrevd tables.
Tables default to those in `local-abbrev-table'.
With \\[universal-argument], edit abbrevd tables related to MODE.
With \\[universal-argument] \\[universal-argument], edit all abbrevd tables.
If TABLE-LIST is non-nil, restrict to abbrevd tables in that list."
  (interactive
   (let ((arg (prefix-numeric-value current-prefix-arg)))
     (list (and (eq 4 arg) (list (intern (nvp-read-mode))))
           (eq 16 arg))))
  (let ((abbrev-table-name-list
         (cond (modes (nvp-abbrevd--mode-tables modes))
               (t (nvp-abbrevd--active-tables
                   (or table-list
                       (and all abbrev-table-name-list)
                       (mapcar #'abbrev-table-name
                               (nvp:local-abbrev-table))))))))
    (if (null abbrev-table-name-list)
        (nvp-abbrev-msg "No abbrevd tables")
      (let* ((inhibit-read-only t)
             (buf (cl-letf (((symbol-function #'edit-abbrevs-mode) #'ignore))
                    (prepare-abbrev-list-buffer))))
        (with-current-buffer buf
          (nvp-abbrevd-edit-mode)
          (setq-local abbrev-table-name-list abbrev-table-name-list)
          (pop-to-buffer (current-buffer)))))))

(defun nvp-abbrevd--update-table (tablename defs &optional docstring &rest props)
  "Update abbrevd TABLENAME with DEFS."
  (when (and docstring props (symbolp docstring))
    (push docstring props) (setq docstring nil))
  (let ((table (symbol-value (intern-soft (symbol-name tablename) nvp-abbrevd-obarray))))
    (unless table
      (user-error "%s not defined" tablename))
    (while (consp props)
      (unless (cdr props) (error "Missing value for property %S" (car props)))
      (abbrev-table-put table (pop props) (pop props)))
    (dolist (elt defs)
      (apply #'define-abbrev table elt))
    table))

(defun nvp-abbrevd-edit-redefine ()
  "Redefine abbrevd tables in buffer with current abbrevs."
  (interactive nil nvp-abbrevd-edit-mode)
  (save-restriction
    (widen)
    (nvp-abbrevd-define-abbrevs t)
    (set-buffer-modified-p nil)))

(defun nvp-abbrevd-define-abbrevs (&optional arg)
  "Wrapper around `define-abbrevs' to update abbrevd tables.
If ARG is non-nil, clear abbrev defs and replace with current."
  (interactive "P" nvp-abbrevd-edit-mode)
  (cl-letf (((symbol-function 'define-abbrev-table) #'nvp-abbrevd--update-table))
    (funcall-interactively #'define-abbrevs arg)))


(defun nvp-abbrevd--write-table-props (table)
  "Write TABLE properties."
  ;; Called after `insert-abbrev-table-description' inserts table definition
  (backward-sexp)
  (down-list)
  (forward-list)
  (insert "\n")
  (dolist (prop '(:enable-function :regexp :file :type :case-fixed :priority :mode))
    (--when-let (abbrev-table-get table prop)
      (insert (format " %S %s%S\n" prop (and (atom it) "'") it)))))

(defun nvp-abbrevd-write-abbrev-file (file &optional verbose)
  "Write abbrevs in buffer to FILE."
  (let ((coding-system-for-write 'utf-8)
        (abbrev-list abbrev-table-name-list))
    (with-temp-buffer
      (let ((abbrev-table-name-list abbrev-list))
        (dolist (table abbrev-table-name-list)
          (when (abbrev--table-symbols table)
            (insert-abbrev-table-description table nil)
            (nvp-abbrevd--write-table-props
             (symbol-value (nvp-abbrevd-get-table table))))))
      (when (unencodable-char-position (point-min) (point-max) 'utf-8)
	(setq coding-system-for-write 'utf-8-emacs))
      (goto-char (point-min))
      (insert (format ";;-*- coding: %s; lexical-binding: t; -*-\n"
                      coding-system-for-write))
      (write-region nil nil file nil (and (not verbose) 0)))))

(defun nvp-abbrevd-edit-save-to-file (file)
  "Redefine current abbrevs and write them to FILE."
  (interactive
   (list (read-file-name "Write abbrevs to: " nvp/abbrevs))
   nvp-abbrevd-edit-mode)
  (nvp-abbrevd-edit-redefine)
  (nvp-abbrevd-write-abbrev-file file t))

(defvar-keymap nvp-abbrevd-edit-mode-map
  :doc "Keymap active in `nvp-abbrevd-edit-mode'."
  "C-c C-c" #'nvp-abbrevd-edit-redefine
  "C-x C-s" #'nvp-abbrevd-edit-save-to-file)

(define-derived-mode nvp-abbrevd-edit-mode edit-abbrevs-mode "Edit-AbbrevD"
  "Editing mode for abbrevd tables.

Commands:
\\<abbrevd-edit-mode-map>"
  :interactive nil
  :abbrev-table nil)


;;; Minor Mode

(defvar-local nvp-abbrevd-friendly-modes nil)

(define-minor-mode nvp-abbrevd-minor-mode
  "Local abbrevds."
  :lighter nil
  :abbrev-table nil
  (if nvp-abbrevd-minor-mode
      (let (nvp-abbrev-verbose)
        (nvp-abbrevd-add-mode-tables
         (cons (or nvp-mode-name major-mode) nvp-abbrevd-friendly-modes)
         t))))

;; FIXME(5/7/24): make more efficient - track modes abbrevds somewhere?
(defun nvp-abbrevd--should-enable-p (modes)
  "Return non-nil if there are interesting abbrevd tables for any of MODES."
  (catch 'done
    (obarray-map
     (lambda (table)
       (and (memq (abbrev-table-get (symbol-value table) :mode) modes)
            (throw 'done t)))
     nvp-abbrevd-obarray)))

(defun nvp-abbrevd-minor-mode-on ()
  (interactive)
  (when (and (not (or noninteractive (eq (aref (buffer-name) 0) ?\s)))
             (nvp-abbrevd--should-enable-p
              (cons (or nvp-mode-name major-mode) nvp-abbrevd-friendly-modes)))
    (nvp-abbrevd-minor-mode 1)))

(define-globalized-minor-mode nvp-abbrevd-mode
  nvp-abbrevd-minor-mode nvp-abbrevd-minor-mode-on
  :group 'abbrev)


;; -------------------------------------------------------------------
;;; Commands

;;;###autoload
(defun nvp-abbrevd-remove-tables (tables &optional clear)
  "Remove local abbrevd TABLES.
With no prefix, use local abbrevd TABLES.
With \\[universal-argument] prompt for TABLES from locals.
With \\[universal-argument] \\[universal-argument], use all abbrevd tables."
  (interactive
   (let ((tables (nvp-abbrevd--active-tables
                  (pcase current-prefix-arg
                    ((or '(4) 'nil) (mapcar #'abbrev-table-name (nvp:local-abbrev-table)))
                    ('(16) abbrev-table-name-list)))))
     (when (and tables (equal current-prefix-arg '(4)))
       (setq tables (nvp:abbrevd-read-tables "Remove: " tables)))
     (list tables (and tables (y-or-n-p "Clear? ")))))
  (if (null tables)
      (nvp-abbrev-msg "No abbrevd tables")
    (setq local-abbrev-table (--filter (not (memq (abbrev-table-name it) tables))
                                       (nvp:local-abbrev-table)))
    (when clear
      (setq abbrev-table-name-list (--filter (not (memq it tables))
                                             abbrev-table-name-list))
      (dolist (table tables)
        (obarray-remove nvp-abbrevd-obarray table)))
    (nvp-abbrev-msg "%sed: %S" (if clear "Clear" "Remov") tables)))


;;;###autoload
(defun nvp-abbrevd-add-mode-tables (modes &optional tables)
  "Add abbrevd TABLES for MODES to local tables.
TABLES defaults to all abbrevd tables defined for MODES.
With \\[universal-argument] prompt for tables to add.
With \\[universal-argument] \\[universal-argument], prompt for mode."
  (interactive
   (let* ((modes (list (if (> (prefix-numeric-value current-prefix-arg) 4)
                           (intern (nvp-read-mode))
                         (or nvp-mode-name major-mode))))
          (tables (nvp-abbrevd--mode-tables modes)))
     (list modes (if (and tables current-prefix-arg)
                     (nvp:abbrevd-read-tables "Add: " tables)
                   tables))))
  (or (listp modes) (setq modes (list modes)))
  (when (eq t tables)
    (setq tables (nvp-abbrevd--mode-tables modes)))
  (if (null tables)
      (nvp-abbrev-msg "No abbrevd tables")
    (let* ((local-tables (nvp:local-abbrev-table))
           (locals (mapcar #'abbrev-table-name local-tables))
           to-add)
      (dolist (table tables)
        (unless (memq table locals)
          (push table to-add)))
      (when to-add
        (setq local-abbrev-table (append (mapcar #'symbol-value to-add) local-tables))
        (nvp-abbrev-msg "Added tables: %S" to-add)))))


;;;###autoload
(defun nvp-abbrevd (mode &rest args)
  "Create dynamic abbrevs for MODE.
ARGS should be a plist with keys BUFFER or FILE, see `nvp-abbrevd-read'."
  (interactive (nvp-abbrevd-read current-prefix-arg))
  (let ((defs (apply #'nvp-abbrevd--make-abbrevs
                     (apply #'nvp-abbrevd-make-args mode args))))
    (if (not defs)
        (nvp-abbrev-msg "No abbrevs found")
      (let* ((mode-table (nvp-abbrevd--local-table mode 'use-default))
             (props (apply #'nvp-abbrevd-table-props mode
                           :tables mode-table args))
             (table (apply #'nvp-abbrevd--populate-table
                           defs nil (symbol-name mode) props)))
        (prog1 table
          (setq nvp-abbrev-completion-need-refresh t)
          (nvp-abbrev-msg "Created %s abbrevs (in %s)"
                          (length defs) (abbrev-table-name table)))))))


(provide 'nvp-abbrev-dynamic)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-abbrev-dynamic.el ends here
