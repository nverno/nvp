;;; nvp-trace.el --- trace -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'trace)
(require 'transient)

(nvp:decls :p (trace-mode tracing) :f (flatten-tree) :v (tracing-minor-mode))

(autoload 'find-library-name "find-func")
(autoload 'nvp-elisp-matching-forms "nvp-elisp" )
(autoload 'trace-tree-disable "trace-tree")
(autoload 'tracing-update-mode-line "tracing")

(defvar nvp-trace-group-alist nil
  "Store trace groups.")

;;; Tracing Mode
(declare-function tracing-update-mode-line "tracing")
(defvar tracing--batch nil)
(when (fboundp 'tracing-enable)
  (tracing-enable))

;;;###autoload
(defun nvp-toggle-inhibit-trace (&optional on)
  (interactive "P")
  (setq inhibit-trace (or on (not inhibit-trace)))
  (message "Tracing %s" (if inhibit-trace "inhibited" "enabled"))
  (and (bound-and-true-p tracing-minor-mode)
       (tracing-update-mode-line)))

(defun nvp-trace-active-p ()
  (and (boundp 'trace-buffer)
       (buffer-live-p (get-buffer trace-buffer))))

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
    ("i" "Inhibit trace" nvp-toggle-inhibit-trace)]
   ["Groups"
    ("g" "Group" nvp-trace-group)
    ("G" "Untrace group" nvp-untrace-group :if-non-nil tracing-minor-mode)
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

(defun nvp-trace--read-groups (prompt &optional force-read untrace)
  "Load trace data and PROMPT for group to trace."
  (unless (bound-and-true-p nvp-trace-group-alist)
    (nvp-trace-load-saved))
  (or (and (null force-read)
           (--when-let (completing-read-multiple prompt nvp-trace-group-alist)
             (cl-loop for g in it
                      collect (assq (intern g) nvp-trace-group-alist))))
      (let ((name (or untrace (read-string "Group name: ")))
            (funcs (read--expression prompt)))
        (and name (push (intern-soft name) funcs))
        (prog1 funcs
          (unless untrace
            (push funcs nvp-trace-group-alist))))))

;;;###autoload
(defun nvp-trace-group (groups &optional foreground)
  "Trace GROUPS of functions defined in `nvp-trace-group-alist'."
  (interactive
   (list (nvp-trace--read-groups "Trace: " current-prefix-arg)
         current-prefix-arg))
  (let ((funcs (if (listp (car groups))
                   (apply #'append (--map (cdr it) groups))
                 (cdr groups)))
        (trace-fn (if foreground
                      #'trace-function-foreground
                    #'trace-function-background))
        (tracing--batch t))
    (dolist (fn funcs)
      (funcall trace-fn fn))
    (tracing-add funcs)))

(defun nvp-untrace-group (funcs)
  "Untrace functions in FUNCS."
  (interactive
   (list (apply #'append
                (--map (cdr it)
                       (nvp-trace--read-groups
                        "Untrace: " current-prefix-arg t)))))
  (let ((tracing--batch t))
    (dolist (fn funcs)
      (untrace-function fn))
    (tracing-add funcs nil t)))

;;;###autoload
(defun nvp-trace-library (library &optional macros filter)
  "Trace all top-level defun-like forms in library.
With \\[universal-argument], trace macros and substs as well.
With \\[universal-argument] \\[universal-argument] prompt for filter."
  (interactive (list (read-library-name) current-prefix-arg
                     (nvp:prefix '>=16 (read-string "Filter: "))))
  (require 'nvp-elisp)                  ;gather all defun-like forms
  (let* ((def-forms (if macros (flatten-tree nvp-trace-defun-forms)
                      (assoc 'defun nvp-trace-defun-forms)))
         (forms
          (with-temp-buffer
            (insert-file-contents (find-library-name library))
            (with-syntax-table emacs-lisp-mode-syntax-table
              (nvp-elisp-matching-forms def-forms))))
         (tracing--batch t))
    (when filter
      (setq forms (--filter (string-match-p filter (symbol-name it)) forms)))
    (dolist (fn forms)
      (trace-function-background fn))
    (tracing-add forms 'library)
    (message "tracing %d funs from %s: %S" (length forms) library forms)))

;;;###autoload
(defun nvp-trace-hooks (&optional library)
  "Trace all hooks defined with `add-hook' in LIBRARY."
  (interactive
   (list (if current-prefix-arg (read-library-name) "nvp-mode-hooks")))
  (require 'nvp-elisp)
  (unless library (setq library "nvp-mode-hooks"))
  (cl-flet ((hook-p
              (form)
              (and (eq (car form) 'add-hook)
                   (eval (nth 2 form)))))
    (let ((forms (with-temp-buffer
                   (insert-file-contents (find-library-name library))
                   (with-syntax-table emacs-lisp-mode-syntax-table
                     (nvp-elisp-matching-forms #'hook-p))))
          (tracing--batch t))
      (dolist (fn forms)
        (trace-function-background fn))
      (tracing-add forms 'hooks)
      (message "tracing hooks: %S" forms))))


(provide 'nvp-trace)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-trace.el ends here
