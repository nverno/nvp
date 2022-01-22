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
(require 'nvp)
(nvp-req 'nvp-abbrev 'subrs)

(nvp-decls :v (nvp-abbrev-completion-need-refresh unicode-latex-abbrev-table))

(defvar nvp-abbrev--read-history ())

;; get list of all table properties, converting parent tables into symbols
(defun nvp-abbrev-get-plist (table)
  (unless (abbrev-table-p table) (setq table (symbol-value table)))
  (when-let* ((sym (obarray-get table ""))
              (props (symbol-plist sym)))
    (cl-loop for (k v) on props by #'cddr
       if (eq :parents k)
       collect (list k (nvp-abbrev--all-parents table 'names))
       else
       collect (list k v))))

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
  "Determine with abbrev table to use based on prefix and possibly context."
  (if (not abbrev) (cl-call-next-method)
    (let* ((abbr (or exp abbrev))
           (table-prefix
            (and abbr
                 (cdr (cl-find-if (lambda (pref) (string-prefix-p pref abbr))
                                  '(("cl-"  . "emacs-lisp-cl")
                                    ("nvp-" . "emacs-lisp-nvp")) :key #'car)))))
      (regexp-quote
       (format "%s-abbrev-table" (or table-prefix local-table major-mode))))))

;; insert starter abbrev table template
(defun nvp-abbrev--insert-template (table &optional parents)
  (nvp-defq parents (if (derived-mode-p 'prog-mode) '(prog-mode)
                      '(fundamental-mode)))
  (let ((parents
         (concat
          "(list "
          (mapconcat (lambda (s) (concat (symbol-name s) "-abbrev-table")) parents " ")
          ")")))
    (insert
     (concat
      (when (zerop (buffer-size))
        ";; -*- coding: utf-8; -*-\n")
      "\n(define-abbrev-table '" table "\n"
      "  '()\n"
      (format "  \"%s Abbrevs.\"\n"
              (capitalize (replace-regexp-in-string "-abbrev-table" "" table)))
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
;;; Commands

;;;###autoload
(defun nvp-abbrev-jump-to-file (arg)
  "Jump to abbrev file, `nvp-abbrev-local-table', and search for insertion
location. Prefix ARG specifies the length of the preceding text to use as
abbrev. When abbrev text is selected, searching is done first by length
then lexically."
  (interactive "P")
  (let* ((local-abbrevs (bound-and-true-p nvp-abbrev-local-table))
         (prefix (cond
                  ((use-region-p)
                   (nvp-abbrev-grab-region (region-beginning) (region-end)))
                  (arg (nvp-abbrev--grab-prev arg))
                  (t nil)))
         (expansion (and (consp prefix) (prog1 (cdr prefix)
                                          (setq prefix (car prefix)))))
         (table (nvp-abbrev-table-name local-abbrevs prefix expansion)))
    (save-restriction
      (with-current-buffer (nvp-abbrev--get-table
                            table (or (bound-and-true-p nvp-abbrev-local-file)
                                      (expand-file-name table nvp/abbrevs)))
        (if (not prefix)      ;blank start: no prefix or expansion
            (progn
              (ignore-errors (down-list))
              (when (y-or-n-p "Expand new snippet?")
                (yas-expand-snippet "(\"$1\" \"$2\" nil :system t)\n")))
          (narrow-to-defun)
          (let ((end (save-excursion (forward-list) (1- (point)))))
            (when prefix
              (while
                  (and (re-search-forward
                        "[^\\(?:(define-\\)](\"\\(\\w+\\)" end 'move)
                       (let ((str (match-string-no-properties 1)))
                         (or (> (length prefix) (length str))
                             (string> prefix str)))))
              ;; insert default template for prefix
              (back-to-indentation)
              (yas-expand-snippet
               (format "(\"%s\" \"%s\" nil :system t)\n" prefix
                       (if expansion (concat "${1:" expansion "}") "$1"))))))
        ;; reload abbrev table after modification
        (cl-labels ((nvp-abbrev-save-hook
                     ()
                     (widen)
                     (quietly-read-abbrev-file (buffer-file-name))
                     ;; refresh cached active abbrev tables
                     (setq nvp-abbrev-completion-need-refresh t)))
          (add-hook 'after-save-hook #'nvp-abbrev-save-hook t 'local))))))

;; add unicode abbrevs to local table parents
;;;###autoload
(defun nvp-abbrev-add-parent (table &optional file)
  "Add abbrev TABLE as parent of `local-abbrev-table'.
If FILE is non-nil, read abbrevs from FILE."
  (interactive
   (list (nvp-abbrev--read-table
          "Add parent abbrev table: "
          (mapcar #'symbol-name abbrev-table-name-list))))
  (when file
    (quietly-read-abbrev-file file))
  (let ((parents (abbrev-table-get local-abbrev-table :parents)))
    (abbrev-table-put
     local-abbrev-table :parents (cons (symbol-value (intern table)) parents)))
  (message "Activated %s abbrevs locally." table))

;;;###autoload
(defun nvp-abbrev-load-unicode (arg)
  "Add unicode abbrevs as parent of local abbrev table.
With prefix, unload unicode abbrevs."
  (interactive "P")
  (if arg
      (nvp-abbrev-remove-parent unicode-latex-abbrev-table)
    (nvp-abbrev-add-parent
     "unicode-latex-abbrev-table"
     (expand-file-name "unicode-latex-abbrev-table" nvp/abbrevs))))

;;;###autoload
(defun nvp-abbrev-remove-parent (table &optional parents)
  "Remove parent TABLE from `local-abbrev-table' PARENTS."
  (interactive
   (let ((parents
          (mapcar #'abbrev-table-name
                  (abbrev-table-get local-abbrev-table :parents))))
     (list (intern (nvp-abbrev--read-table
                    "Remove local abbrev parent: " (mapcar #'symbol-name parents)))
           parents)))
  (let ((new-p (mapcar #'symbol-value (remq table parents))))
    (abbrev-table-put local-abbrev-table :parents new-p))
  (message "Removed local %S abbrevs."
           (if (abbrev-table-p table) (abbrev-table-name table) table)))

;; write abbrev table
;; temporarily rebind `abbrev--write' to write :system abbrevs
;;;###autoload
(defun nvp-abbrev-write-abbrev-table (table file)
  "Write abbrev TABLE to FILE as :system abbrevs."
  (interactive
   (list
    (if current-prefix-arg
        (abbrev-table-name local-abbrev-table)
      (nvp-abbrev--read-table
       "Abbrev table: " (mapcar #'symbol-name (nvp-abbrev--nonempty))))
    (read-file-name "Write abbrevs to: ")))
  (let ((abbrev-table-name-list (list (intern table))))
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
  (interactive
   (list
    (if current-prefix-arg
        (abbrev-table-name local-abbrev-table)
      (nvp-abbrev--read-table
       "List abbrev table props: "
       (mapcar #'symbol-name (nvp-abbrev--nonempty))))))
  (let ((props (nvp-abbrev-get-plist (intern table))))
    (nvp-with-results-buffer nil table
      (pcase-dolist (`(,k ,v) props)
        (princ (format "%S: %S\n" k v))))))

;; unlike default, list all parent tables and dynamic tables as well
;;;###autoload
(defun nvp-abbrev-list-abbrevs (&optional all)
  "List all local abbrev tables by default.
If ALL is non-nill, list all abbrev tables."
  (interactive "P")
  (if all
      (display-buffer (prepare-abbrev-list-buffer))
    (let ((abbrev-table-name-list
           (cons (abbrev-table-name local-abbrev-table)
                 (nvp-abbrev--all-parents local-abbrev-table 'names))))
      (and nvp-abbrev-dynamic-table
           (push 'nvp-abbrev-dynamic-table abbrev-table-name-list))
      (display-buffer (prepare-abbrev-list-buffer nil)))))

(provide 'nvp-abbrev)
;;; nvp-abbrev.el ends here
