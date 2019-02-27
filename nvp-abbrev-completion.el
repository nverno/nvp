;;; nvp-abbrev-completion.el --- local abbrev completion -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-26 00:20:22>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created:  6 February 2019

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

(provide 'nvp-abbrev-completion)
;;; nvp-abbrev-completion.el ends here
