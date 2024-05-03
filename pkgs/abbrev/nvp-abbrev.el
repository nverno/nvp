;;; nvp-abbrev.el --- abbrev commands -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Jumping to abbrev files
;; - converts region to abbrev if region is active (generic)
;;   includes default + elisp
;; - uses previous N chars with prefix N
;; - determines table to insert into given abbrev (generic)
;;   includes default + elisp
;; - sorts tables by abbrev length then lexi and expands new abbrev at proper
;;   location
;; - prefers buffer-local abbrev file over default
;; - reloads abbrevs on save
;; - inserts new abbrev table when necessary
;;
;; Additional commands
;; - add / remove abbrev-table as parent of local table
;; - activate unicode abbrevs locally
;; - some enhancements for listing tables
;; - show all abbrev-table properties, including all parents recursively
;; - when listing locally active table, include all parents of local-table
;; - write abbrevs as system abbrevs to save permanently
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'abbrev)
(nvp:decls :v (nvp-abbrev-completion-need-refresh unicode-latex-abbrev-table))


(defvar nvp-abbrev-verbose t
  "Non-nil to say stuff.")

(defvar nvp-abbrevd-obarray (obarray-make)
  "Obarray for on-the-fly local abbrev tables.")

(defvar nvp-abbrev--read-history ())

(defvar-local nvp-abbrev-splitters "[:/_-]")

(defvar-local nvp-abbrev-joiner "/")

;; Dont ever call this! - it's too easy to wipeout abbrevs
(advice-add 'abbrev-edit-save-buffer :override #'ignore)


(defun nvp-abbrev--lisp-transformer (str &optional joiner splitters)
  "Transform STR to abbrev by splitting on SPLITTERS and join with JOINER."
  (--mapcc (if (string-empty-p it)
               (or joiner nvp-abbrev-joiner)
             (substring it 0 1))
           (split-string str (or splitters nvp-abbrev-splitters)) ""))

(defsubst nvp-abbrev--table-value (table)
  (if (symbolp table) (symbol-value table) table))

;; FIXME: returning string was for previous stuff
(defsubst nvp-abbrev--table-name (table)
  (if (symbolp table) (symbol-name table) "Dynamic"))


(defsubst nvp-abbrev--add-local (table)
  "Add TABLE to `local-abbrev-table'."
  (unless (or (eq table local-abbrev-table)
              (and (listp local-abbrev-table)
                   (memq table local-abbrev-table)))
    (setq-local local-abbrev-table
                (cond ((null local-abbrev-table) table)
                      ((listp local-abbrev-table)
                       (cons table local-abbrev-table))
                      (t (list table local-abbrev-table))))))

(defsubst nvp-abbrev--remove-local (table)
  "Remove TABLE from `local-abbrev-table'."
  (setq-local local-abbrev-table
              (cond ((null local-abbrev-table) nil)
                    ((eq table local-abbrev-table) nil)
                    ((listp local-abbrev-table)
                     (remq table local-abbrev-table))
                    (t local-abbrev-table))))


;; get list of all table properties, converting parent tables into symbols
(defun nvp-abbrev-get-plist (table)
  (setq table (nvp-abbrev--table-value table))
  (when-let* ((sym (obarray-get table ""))
              (props (symbol-plist sym)))
    (cl-loop for (k v) on props by #'cddr
             if (eq :parents k)
             collect (list k (nvp-abbrev--all-parents table 'names))
             else
             collect (list k v))))


;;; XXX(5/2/24): check for dependency loop? hasnt ever happened, but maybe...
(defun nvp-abbrev--all-parents (tables &optional symbols)
  "Retun list of all parents of TABLES, recursively.
If SYMBOLS, return table symbols"
  (or (listp tables) (setq tables (list tables)))
  (let (res table)
    (while (setq table (pop tables))
      (or (abbrev-table-p table) (setq table (symbol-value table)))
      (let ((parents (abbrev-table-get table :parents)))
        (while parents
          (setq res (append res parents))
          (setq parents (car (--map (abbrev-table-get it :parents) parents))))))
    (if symbols
        (mapcar #'abbrev-table-name res)
      res)))


(defun nvp-abbrev--read-table
    (&optional prompt collection local predicate return-table)
  "Read abbrev table from COLLECTION or LOCAL abbrevs.
COLLECTION defaults to `abbrev-table-name-list'.
PROMPT and PREDICATE are passed to `completing-read'.
If RETURN-TABLE is non-nil, return table instead of its symbol."
  (let ((tables
         (cond (local (if (listp local-abbrev-table)
                          ;; XXX: can all be unnamed?
                          (--map (abbrev-table-name it) local-abbrev-table)
                        (or (abbrev-table-name local-abbrev-table)
                            local-abbrev-table)))
               (t (or collection
                      abbrev-table-name-list)))))
    (when (listp tables)
      (let ((str (completing-read
                  (or prompt "Abbrev table: ")
                  tables predicate t nil 'nvp-abbrev--read-history)))
        (setq tables (or (intern-soft str nvp-abbrevd-obarray)
                         (intern str)))))
    (or tables (user-error "No abbrev tables"))
    (if return-table
        (symbol-value tables)
      tables)))


;; -------------------------------------------------------------------
;;; Generics

(cl-defgeneric nvp-abbrev-grab-region (beg end)
  "Generic function to return abbrev from region BEG to END .
Should return sexp of form (abbrev                          . expansion)."
  (let ((exp (buffer-substring-no-properties beg end)))
    (cons (read-string (format "Abbrev for %s: " exp)) exp)))

(cl-defmethod nvp-abbrev-grab-region
  (beg end &context (major-mode emacs-lisp-mode))
  "Try to determine appropriate elisp abbrev from region.
Remove any surrounding parens and use first chars with lisp transformer.
With prefix, don't split region by whitespace."
  (let* ((str (if current-prefix-arg (buffer-substring-no-properties beg end)
                (car (split-string
                      (buffer-substring-no-properties beg end) nil 'omit))))
         (exp (string-trim str "[ \t\n\(]" "[ \t\n\)]"))
         (trans (nvp-abbrev--lisp-transformer exp)))
    (if (string-prefix-p "cl-" exp)
        (setq trans (concat "cl" (substring trans 1))))
    (cons trans exp)))

(cl-defgeneric nvp-abbrev-table-name (&optional local-table _abbrev _exp)
  "Generic function to return the name of abbrev file to use with given abbrev
or expansion."
  (regexp-quote (format "%s-abbrev-table" (or local-table major-mode))))

(cl-defmethod nvp-abbrev-table-name
  (&context (major-mode emacs-lisp-mode) &optional local-table abbrev exp)
  "Determine which abbrev table to use based on prefix and possibly context."
  (if (not abbrev)
      (cl-call-next-method)
    (let* ((abbr (or exp abbrev))
           (table-prefix
            (and abbr (cdr (cl-find-if (lambda (pref) (string-prefix-p pref abbr))
                                       '(("cl-"  . "emacs-lisp-cl")
                                         ("nvp-" . "emacs-lisp-nvp"))
                                       :key #'car)))))
      (regexp-quote
       (format "%s-abbrev-table" (or table-prefix local-table major-mode))))))

;; insert starter abbrev table template
(defun nvp-abbrev--insert-template (table &optional parents)
  (or parents (setq parents (if (derived-mode-p 'prog-mode)
                                '(prog-mode)
                              '(fundamental-mode))))
  (let ((parents
         (concat "(list " (--mapcc (format "%s-abbrev-table" it) parents) ")")))
    (insert (concat (and (zerop (buffer-size)) ";; -*- coding: utf-8; -*-\n")
                    "\n(define-abbrev-table '" table "\n"
                    "  '()\n"
                    (format "  \"%s Abbrevs.\"\n"
                            (capitalize
                             (replace-regexp-in-string "-abbrev-table" "" table)))
                    (format "  :enable-function #'nvp-abbrev-expand-p\n")
                    (format "  :parents %s)" parents)))))

;; open abbrev file and search for the specified table
;; if it doesn't exist insert starter template
(defun nvp-abbrev--get-table (table file)
  (find-file-other-window file)
  (widen)
  (goto-char (point-min))
  (unless (search-forward-regexp (concat "'" table "\\>") nil t)
    (goto-char (point-max))
    (nvp-abbrev--insert-template table))
  (current-buffer))


;;;###autoload
(define-derived-mode abbrev-table-mode emacs-lisp-mode "Abbrev-Table"
  "Simple abbrev table extension mode."
  :abbrev-table nil
  (setq-local imenu-generic-expression
              '((nil "^(define-abbrev-table '\\([^ \n]+\\)" 1))))


;; -------------------------------------------------------------------
;;; Jump

(defun nvp-abbrev--grab-prev (nchars)
  "Grab preceding NCHARS to match against in abbrev table."
  (save-excursion
    (let ((end (point))
          (_ (skip-chars-backward nvp-abbrev-prefix-chars (- (point) nchars)))
          (start (point)))
      (buffer-substring-no-properties start end))))

;;;###autoload
(defun nvp-abbrev-jump-to-file (arg)
  "Jump to abbrev file, `nvp-local-abbrev-table', and search for insertion
location. Prefix ARG specifies the length of the preceding text to use as
abbrev. When abbrev text is selected, searching is done first by length
then lexically."
  (interactive "P")
  (let* ((local-abbrevs (bound-and-true-p nvp-local-abbrev-table))
         (prefix (cond ((use-region-p)
                        (nvp-abbrev-grab-region (region-beginning) (region-end)))
                       (arg (nvp-abbrev--grab-prev arg))
                       (t nil)))
         (expansion (and (consp prefix) (prog1 (cdr prefix)
                                          (setq prefix (car prefix)))))
         (table (nvp-abbrev-table-name local-abbrevs prefix expansion)))
    (save-restriction
      (with-current-buffer (nvp-abbrev--get-table
                            table (or (bound-and-true-p nvp-local-abbrev-file)
                                      (expand-file-name table nvp/abbrevs)))
        (if (not prefix)                ; blank start: no prefix or expansion
            (progn (ignore-errors (down-list))
                   (when (y-or-n-p "Expand new snippet?")
                     (yas-expand-snippet "(\"$1\" \"$2\" nil :system t)\n")))
          (narrow-to-defun)
          (let ((end (save-excursion (forward-list) (1- (point)))))
            (when prefix
              (while (and (re-search-forward
                           "[^\\(?:(define-\\)](\"\\(\\w+\\)" end 'move)
                          (let ((str (match-string-no-properties 1)))
                            (or (length> prefix (length str))
                                (string> prefix str)))))
              ;; insert default template for prefix
              (back-to-indentation)
              (yas-expand-snippet
               (format "(\"%s\" \"%s\" nil :system t)\n" prefix
                       (if expansion (concat "${1:" expansion "}") "$1"))))))
        ;; reload abbrev table after modification
        (cl-labels ((nvp-abbrev-save-hook ()
                      (widen)
                      (quietly-read-abbrev-file (buffer-file-name))
                      ;; refresh cached active abbrev tables
                      (setq nvp-abbrev-completion-need-refresh t)))
          (add-hook 'after-save-hook #'nvp-abbrev-save-hook t 'local))))))


;; -------------------------------------------------------------------
;;; Abbrev Tables

;;;###autoload
(defun nvp-abbrev-add-parent (table parent &optional file)
  "Add abbrev PARENT as parent of TABLE.
Table defaults to `local-abbrev-table'.
If FILE is non-nil, read abbrevs from FILE."
  (interactive
   (list (nvp-abbrev--read-table "Table: " nil 'local nil 'table)
         (nvp-abbrev--read-table
          "Parent: " (mapcar #'symbol-name abbrev-table-name-list)
          nil nil 'table)))
  (when file
    (quietly-read-abbrev-file file))
  (let ((parents (abbrev-table-get table :parents)))
    (unless (memq parent parents)
      (abbrev-table-put table :parents (cons parent parents))
      (when nvp-abbrev-verbose
        (message "Added %s" (abbrev-table-name parent))))))


;;;###autoload
(defun nvp-abbrev-load-unicode (arg)
  "Add unicode abbrevs as local abbrev table.
With prefix, unload unicode abbrevs."
  (interactive "P")
  (if arg (--when-let (bound-and-true-p unicode-latex-abbrev-table)
            (nvp-abbrev--remove-local it))
    (quietly-read-abbrev-file
     (expand-file-name "unicode-latex-abbrev-table" nvp/abbrevs))
    (cl-assert (bound-and-true-p unicode-latex-abbrev-table))
    (nvp-abbrev--add-local unicode-latex-abbrev-table)))


;;;###autoload
(defun nvp-abbrev-remove-parent (table to-remove &optional parents)
  "Remove TO-REMOVE from TABLE's parents.
PARENTS can provide current parents."
  (interactive
   (let* ((table (nvp-abbrev--read-table "Table: " nil 'local nil 'table))
          (parents (--map (abbrev-table-name it)
                          (abbrev-table-get table :parents))))
     (list (nvp-abbrev--read-table "Remove parent: " parents)
           parents)))
  (let ((new-p (mapcar #'symbol-value (remq to-remove parents))))
    (abbrev-table-put (nvp-abbrev--table-value table) :parents new-p))
  (when nvp-abbrev-verbose
    (message "Removed %S" to-remove)))


;;;###autoload
(defun nvp-abbrev-write-abbrev-table (table file)
  "Write abbrev TABLE to FILE as :system abbrevs."
  (interactive
   (list (nvp-abbrev--read-table nil nil current-prefix-arg)
         (read-file-name "Write abbrevs to: ")))
  (let ((abbrev-table-name-list (list table)))
    ;; write abbrev table
    ;; temporarily rebind `abbrev--write' to write :system abbrevs
    (cl-letf (((symbol-function 'abbrev--write)
               (lambda (sym)
                 (unless (null (symbol-value sym))
                   (insert "    (")
                   (prin1 (symbol-name sym))
                   (insert " ")
                   (prin1 (symbol-value sym))
                   (insert " ")
                   (prin1 (symbol-function sym))
                   (insert " :system t)\n")))))
      (write-abbrev-file file))))


;;;###autoload
(defun nvp-abbrev-properties (table)
  "List all abbrev table properties."
  (interactive (list (nvp-abbrev--read-table nil nil current-prefix-arg)))
  (let ((props (nvp-abbrev-get-plist table))
        (name (nvp-abbrev--table-name table)))
    (nvp:with-results-buffer :title name
      (pcase-dolist (`(,k ,v) props)
        (princ (format "%S: %S\n" k v))))))


;; unlike default, list all parent tables and dynamic tables as well
;;;###autoload
(defun nvp-abbrev-list-abbrevs (&optional all)
  "List all major-mode and local tables active in buffer.
If ALL is non-nil, list all abbrev tables."
  (interactive "P")
  (if all (display-buffer (prepare-abbrev-list-buffer))
    (let* ((locals (--map (abbrev-table-name it)
                          (if (listp local-abbrev-table)
                              local-abbrev-table
                            (list local-abbrev-table))))
           (abbrev-table-name-list
            (append locals (nvp-abbrev--all-parents local-abbrev-table 'names))))
      (display-buffer (prepare-abbrev-list-buffer nil)))))


;; -------------------------------------------------------------------
;;; Menu

(require 'transient)

(transient-define-infix nvp-abbrev-menu--splitters ()
  :class 'transient-lisp-variable
  :variable 'nvp-abbrev-splitters)

(transient-define-infix nvp-abbrev-menu--joiner ()
  :class 'transient-lisp-variable
  :variable 'nvp-abbrev-joiner)

(nvp:transient-toggle nvp-abbrev-menu nvp-abbrev-verbose)

;;;###autoload(autoload 'nvp-abbrev-menu "nvp-abbrev-dynamic" nil t)
(transient-define-prefix nvp-abbrev-menu ()
  [["Create Abbrevs"
    ("x" "From buffer/file" nvp-abbrevd)
    ""
    "Settings"
    (":j" "Joiner" nvp-abbrev-menu--joiner)
    (":s" "Splitters" nvp-abbrev-menu--splitters)
    (":v" "Verbose" nvp-abbrev-menu--toggle-nvp-abbrevd-verbose)]
   ["Tables"
    ("s" "Save table" nvp-abbrev-write-abbrev-table)
    ("R" "Remove parent" nvp-abbrev-remove-parent)
    ("P" "Add parent" nvp-abbrev-add-parent)
    ("u" "Load unicode" nvp-abbrev-load-unicode)]
   ["Info"
    ("l" "List active" nvp-abbrev-list-abbrevs)
    ("p" "Table Props" nvp-abbrev-properties)]])


(provide 'nvp-abbrev)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-abbrev.el ends here
