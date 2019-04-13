;;; nvp-abbrev-completion.el --- local abbrev completion -*- lexical-binding: t; -*-

;;; Commentary:

;; Completion functions for abbrevs that take into account local context
;; using each active table's :regexp and :enable-function properties
;;
;; These complete for all parents of `local-abbrev-table', accounting for
;; :enable-function and :regexp properties for each table separately
;;
;; Used to produce completion candidates for company and hippie-exp.

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(require 'abbrev)
(require 'nvp)
(require 'nvp-abbrev-util)

;; if non-nil, update active table cache
(defvar-local nvp-abbrev-completion-need-refresh nil)

;; use local table along with its parents + global table
(nvp-define-cache nvp-abbrev-completion--tables () nil
  :cache nvp-abbrev-completion--tables-cache
  :local t
  :predicate (not nvp-abbrev-completion-need-refresh)
  (prog1 (nvp-abbrev--active-tables)
    (setq nvp-abbrev-completion-need-refresh nil)))

;; add completion annotations
(defun nvp-abbrev-completion--apply-annotation (table)
  (let ((tab-name (symbol-name table)))
    (mapatoms (lambda (sym) (add-text-properties 0 1 (list 'annotation tab-name) sym)))))

;; active tables in current context, determined by :enable-function
(defun nvp-abbrev-completion--active-tables ()
  (cl-remove-if-not (lambda (tab)
                      (let ((pred (abbrev-table-get
                                   (symbol-value tab) :enable-function)))
                        (if pred (funcall pred) t)))
                    (nvp-abbrev-completion--tables)))

;; default grab previous abbrev when no regexp -- see `abbrev--before-point'
(defsubst nvp-abbrev-completion--grab ()
  (let ((lim (point)) start end)
    (backward-word 1)
    (setq start (point))
    (forward-word 1)
    (setq end (min (point) lim))
    (list (buffer-substring start end) start end)))

;; Return first prefix from tables that satisfies its `:enable-function'
;; and matches its table's `:regexp'
(defun nvp-abbrev-completion--prefix ()
  (cl-loop for tab in (nvp-abbrev-completion--active-tables)
     as re = (abbrev-table-get (symbol-value tab) :regexp)
     if (not re)
     return (nvp-abbrev-completion--grab)
     when (looking-back re (line-beginning-position))
     return (list (match-string-no-properties 1) (match-beginning 0) (point))))

;; return beginning position of prefix for hippie
(defun nvp-abbrev-completion-prefix-beg ()
  (when-let ((abbrev (nvp-abbrev-completion--prefix)))
    (cadr abbrev)))

;; return prefix, either matching a table's predicates or defaulting to the
;; previous symbol
(defun nvp-abbrev-completion-prefix ()
  (or (car-safe (nvp-abbrev-completion--prefix))
      (nvp-grab-symbol)))

;; Return completion candidates, taking into account per-table :regexp
(defun nvp-abbrev-completion-candidates (arg)
  (cl-loop for tab in (nvp-abbrev-completion--active-tables)
     nconc (delete "" (all-completions arg (symbol-value tab)))))

;; -------------------------------------------------------------------
;;; Abbrevs
(require 'hippie-exp)

;; #<marker at 30147 in hippie-exp.el.gz>
;;;###autoload
(defun nvp-he-try-expand-local-abbrevs (old)
  "Try to expand word from locally active abbrev tables.
Accounts for :enable-function and :regexp table properties when selecting
candidates."
  (cl-block nil
    (unless old
      (let ((beg (nvp-abbrev-completion-prefix-beg)))
        (and (not beg) (cl-return))
        (he-init-string beg (point))
        (unless (he-string-member he-search-string he-tried-table)
          (setq he-tried-table (cons he-search-string he-tried-table)))
        (setq he-expand-list                    ;expansion candidates
              (and (not (equal he-search-string ""))
                   (delq nil
                         (mapcan
                          (lambda (table)
                            (mapcar
                             (lambda (prefix)
                               (let ((exp
                                      (abbrev-expansion prefix (symbol-value table))))
                                 (if (vectorp exp)
                                     (aref exp 0)  ;expand hooks
                                   exp)))
                             (all-completions he-search-string (symbol-value table))))
                          (nvp-abbrev-completion--active-tables)))))))
    (while (and he-expand-list                  ;clean expansion list
                (he-string-member (car he-expand-list) he-tried-table t))
      (setq he-expand-list (cdr he-expand-list)))
    (prog1 (not (null he-expand-list))
      (if (null he-expand-list)
          (and old (he-reset-string))
        (he-substitute-string (pop he-expand-list) t)))))

(provide 'nvp-abbrev-completion)
;;; nvp-abbrev-completion.el ends here
