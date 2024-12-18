;;; nvp-trace.el --- trace -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'trace)
(require 'transient)
(nvp:decls :p (trace-mode tracing) :f (flatten-tree))

(autoload 'find-library-name "find-func")
(autoload 'nvp-elisp-matching-forms "nvp-elisp" )
(autoload 'trace-tree-disable "trace-tree")

(defvar nvp-trace-default-background t
  "If non-nil, trace everying in the background by default.")

(defvar nvp-trace-group-alist nil
  "Store trace groups.")

;;; Tracing Mode
(defvar tracing--batch)
(when (fboundp 'tracing-enable)
  (tracing-enable t))

(defsubst nvp-trace-active-p ()
  (or (and (fboundp 'tracing--active-p)
           (tracing--active-p))
      (and (boundp 'trace-buffer)
           (buffer-live-p (get-buffer trace-buffer)))))

(defun nvp-trace-load-saved (&optional clobber)
  "Load group configurations.
With prefix, CLOBBER current `nvp-trace-group-alist'."
  (interactive "P")
  (let ((group-alist nvp-trace-group-alist))
    (load-file (expand-file-name "trace-data.el" nvp/data))
    (message "Loaded %d groups" (length nvp-trace-group-alist))
    (unless clobber
      (setq nvp-trace-group-alist
            (seq-uniq (append group-alist nvp-trace-group-alist)
                      #'equal)))))

;;;###autoload(autoload 'nvp-trace-menu "nvp-trace" nil t)
(transient-define-prefix nvp-trace-menu ()
  "Trace"
  [["Trace"
    ("f" "Defun" trace-function)
    ("b" "Defun background" trace-function-background)
    ("u" "Untrace defun" untrace-function)
    ("q" "Untrace all" untrace-all)
    "--"
    ("i" "Inhibit trace" trace-mode-toggle-inhibit)]
   ["Groups"
    ("g" "Group" nvp-trace-group)
    ("G" "Untrace group" nvp-untrace-group-or-funs)
    ("r" "Regexp" nvp-trace-regexp)
    ("l" "Library" nvp-trace-library)
    ("h" "Hooks" nvp-trace-hooks)
    "--"
    ("L" "Load saved" nvp-trace-load-saved)]
   ["Results" :if nvp-trace-active-p
    ("j" "Display" trace-mode-display-results)
    ("k" "Clear" trace-mode-clear)]
   ["Tree mode" :if (lambda () (featurep 'trace-tree))
    ("tt" "Enable" trace-tree-enable)
    ("tq" "Disable" trace-tree-disable)]])


;; -------------------------------------------------------------------
;;; Groups

(defvar nvp-trace-defun-forms
  '((defun cl-defun declare-function autoload cl-defmethod t)
    (defmacro cl-defmacro)
    (defsubst cl-defsubst)))

(defsubst nvp--trace-matching-funs (regexp &optional untrace)
  "Get functions matching REGEXP.
When UNTRACE, only return matches that are currently traced."
  (cl-loop for sym being the symbols
           when (and (fboundp sym)
                     (or (null untrace) (trace-is-traced sym))
                     (string-match-p regexp (symbol-name sym)))
           collect sym))

(defsubst nvp--trace-library-forms (library &optional match-forms)
  "Get forms from LIBRARY matching MATCH-FORMS."
  (require 'nvp-elisp)                  ; gather all defun-like forms
  (with-temp-buffer
    (insert-file-contents (find-library-name library))
    (with-syntax-table emacs-lisp-mode-syntax-table
      (nvp-elisp-matching-forms match-forms))))

(autoload 'help--symbol-completion-table "help-fns")
(defvar nvp--trace-fn-table
  (apply-partially #'completion-table-with-predicate
                   #'help--symbol-completion-table
                   (lambda (sym)
                     (or (fboundp sym)
                         (get sym 'function-documentation)))
                   t))

(defun nvp--trace-read-group-or-funs (&optional prompt force-read untrace)
  "Load trace data and PROMPT for group to trace."
  (or prompt (setq prompt (format "%srace group/function(s): "
                                  (if untrace "Un" "T"))))
  (or (bound-and-true-p nvp-trace-group-alist)
      (nvp-trace-load-saved))
  (or (unless force-read
        (--> (completing-read-multiple
              prompt (completion-table-in-turn
                      (completion-table-dynamic
                       (lambda (_s) nvp-trace-group-alist))
                      nvp--trace-fn-table))
             (cl-loop for g in it
                      for sym = (intern g)
                      collect (or (assq sym nvp-trace-group-alist) sym))))
      (let ((name (or untrace (read-string "Group name: ")))
            (funcs (completing-read-multiple
                    "Function(s): " nvp--trace-fn-table)))
        (and name (push (intern-soft name) funcs))
        (unless untrace
          (push funcs nvp-trace-group-alist))
        funcs)))

(defsubst nvp--trace-do-batch (forms &optional untrace foreground msg)
  "Trace or UNTRACE FORMS.
Add MSG to the default message reported."
  (let ((tracing--batch t)
        (trace-func (cond (untrace #'untrace-function)
                          (foreground #'trace-function-foreground)
                          (t #'trace-function-background))))
    (dolist (fn forms)
      (funcall trace-func fn))
    (and (fboundp 'tracing-add)
         (tracing-add forms untrace))
    (message "%sracing %s(n=%d): %S"
             (if untrace "Unt" "T") msg (length forms) forms)))

;;;###autoload
(defun nvp-trace-group (groups &optional foreground untrace)
  "Trace GROUPS of functions defined in `nvp-trace-group-alist'.
With \\[universal-argument], trace in opposite of
`nvp-trace-default-background'. With prefix `<=0', UNTRACE."
  (interactive (let* ((raw (prefix-numeric-value current-prefix-arg))
                      (arg (abs raw))
                      (untrace (<= raw 0)))
                 (list (nvp--trace-read-group-or-funs nil (> arg 1) untrace)
                       (if (> arg 1) nvp-trace-default-background
                         (not nvp-trace-default-background))
                       untrace)))
  (let* ((names)
         (groups (--mapcat (if (listp it)
                               (progn (push (car it) names) (cdr it))
                             (list it))
                           groups)))
    (nvp--trace-do-batch
     groups untrace foreground
     (and names (--mapcc (symbol-name it) names ", ")))))

;;;###autoload
(defun nvp-untrace-group-or-funs (forms)
  "Untrace FORMS.
Interactively, prompt for groups or functions to untrace.
With \\[universal-argument], prompt for list of functions."
  (interactive (list (nvp--trace-read-group-or-funs
                      nil current-prefix-arg t)))
  (nvp-trace-group forms nil t))

;;;###autoload
(defun nvp-trace-regexp (regexp &optional foreground untrace)
  "Trace or UNTRACE functions matching REGEXP.
With prefix `<=0', UNTRACE."
  (interactive (let ((raw (prefix-numeric-value current-prefix-arg)))
                 (list (read-regexp
                        (format "%srace Regexp: " (if (<= raw 0) "Un" "T")))
                       (not nvp-trace-default-background)
                       (<= raw 0))))
  (nvp--trace-do-batch
   (nvp--trace-matching-funs regexp)
   untrace foreground (format "\"%s\" matches" regexp)))

;;;###autoload
(defun nvp-trace-library (library &optional macros filter foreground untrace)
  "Trace or UNTRACE top-level defun-like forms in library.
With \\[universal-argument], trace macros and substs as well.
With \\[universal-argument] \\[universal-argument] prompt for filter.
With prefix `<=0', UNTRACE forms."
  (interactive (let* ((raw (prefix-numeric-value current-prefix-arg))
                      (arg (abs raw)))
                 (list (read-library-name) (> arg 1)
                       (and (>= arg 16) (read-string "Filter forms by: "))
                       (not nvp-trace-default-background)
                       (<= raw 0))))
  (let* ((unfiltered-forms
          (nvp--trace-library-forms
           library (if macros (flatten-tree nvp-trace-defun-forms)
                     (assoc 'defun nvp-trace-defun-forms))))
         (forms
          (if filter (--filter (string-match-p filter (symbol-name it))
                               unfiltered-forms)
            unfiltered-forms)))
    (nvp--trace-do-batch
     forms untrace foreground
     (format "%s func%ss" library (if macros "/mac/subr" "")))))

;;;###autoload
(defun nvp-trace-hooks (&optional library foreground untrace)
  "Trace or UNTRACE all hooks defined with `add-hook' in LIBRARY.
With prefix `<=0', UNTRACE forms."
  (interactive
   (let ((arg (prefix-numeric-value current-prefix-arg)))
     (list (if (> (abs arg) 1) (read-library-name) "nvp-mode-hooks")
           (not nvp-trace-default-background)
           (<= arg 0))))
  (nvp--trace-do-batch
   (nvp--trace-library-forms
    (or library "nvp-mode-hooks")
    (lambda (form)
      (and (eq (car form) 'add-hook)
           (eval (nth 2 form)))))
   untrace foreground "hooks"))


(provide 'nvp-trace)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-trace.el ends here
