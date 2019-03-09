;;; nvp-neo.el --- neo4j -*- lexical-binding: t; -*-

;; Last modified: <2019-03-09 06:22:36>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 12 September 2017

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(declare-function n4js-start "n4js")
(declare-function n4js-switch-to-buffer "n4js")
(autoload 'nvp-abbrev-expand-p "nvp-abbrev")

;; -------------------------------------------------------------------
;;; Util

;; Find major.minor version, eg. 3.2
(defvar nvp-neo4j-version nil)
(defun nvp-neo4j-version ()
  (or nvp-neo4j-version
      (let ((ver (car (process-lines "neo4j" "--version"))))
        (when (and ver (string-match "\\s-+\\([0-9]+.[0-9]+\\)" ver))
          (setq nvp-neo4j-version (match-string 1 ver))))))

;; -------------------------------------------------------------------
;;; Proc

(defvar nvp-neo4j-buffer "*neo4j*")

;; start or return neo4j process
;; (defun nvp-neo4j-process ()
;;   (interactive)
;;   (let ((buffer (comint-check-proc nvp-neo4j-buffer)))))

;; -------------------------------------------------------------------
;;; REPL

(defvar nvp-neo4j-repl-buffer "*neo4j-shell*")
(defvar nvp-neo4j--last-buffer)

(defun nvp-neo4j-switch-to-buffer ()
  (interactive)
  (if (comint-check-proc nvp-neo4j-repl-buffer)
      (pop-to-buffer nvp-neo4j-repl-buffer)
    (n4js-start)))

(defun nvp-neo4j-switch-to-repl ()
  (interactive)
  (if (and (eq major-mode 'n4js-mode)
           nvp-neo4j--last-buffer)
      (pop-to-buffer nvp-neo4j--last-buffer)
    (setq nvp-neo4j--last-buffer (current-buffer))
    (nvp-neo4j-switch-to-buffer)))

;; -------------------------------------------------------------------
;;; Completion

(eval-when-compile
  (defvar company-dabbrev-code-ignore-case)
  (defvar company-dabbrev-ignore-case)
  (defvar cypher-clauses))
(declare-function company-in-string-or-comment "company")
(declare-function company-grab-symbol "company")
(declare-function company-begin-backend "company")

;; from cypher-mode
(defvar nvp-cypher-completion-words
  (append
   (mapcar
    'upcase
    '(;; cypher-clauses
      "case" "create" "delete" "foreach" "load csv" "match" "merge" "on" "remove"
      "return" "set" "start" "union" "unwind" "using periodic commit" "using"
      "when" "where" "with"
      ;; cypher-keywords
      "all" "allshortestpaths" "and" "any" "as" "asc" "ascending" "assert" "by"
      "case" "constraint on" "count" "create constraint on" "create index on"
      "create unique" "create" "delete" "desc" "descending" "distinct"
      "drop constraint on" "drop index on" "drop" "else" "end" "extract" "false"
      "fieldterminator" "filter" "foreach" "from" "has" "in" "is not null"
      "is null" "is unique" "is" "limit" "load csv" "match" "merge" "node" "none"
      "not" "null" "on create" "on match" "on" "optional match" "or" "order by"
      "reduce" "rel" "relationship" "remove" "return distinct" "return" "scan"
      "set" "shortestpath" "single" "skip" "start" "then" "true" "union all"
      "union" "unique" "unwind" "using index" "using periodic commit" "using scan"
      "when" "where" "with distinct" "with headers" "with" "xor"))
   ;; cypher-functions
   '("abs" "acos" "asin" "atan" "atan2" "avg" "ceil" "coalesce" "collect" "cos"
     "cot" "count" "degrees" "e" "endnode" "exists" "exp" "floor" "has"
     "haversin" "head" "id" "labels" "last" "left" "length" "log" "log10"
     "lower" "ltrim" "max" "min" "nodes" "percentilecont" "percentiledisc" "pi"
     "radians" "rand" "range" "reduce" "relationships" "rels" "replace" "right"
     "round" "rtrim" "sign" "sin" "size" "split" "sqrt" "startnode" "stdev"
     "stdevp" "str" "substring" "sum" "tail" "tan" "timestamp" "tofloat" "toint"
     "tolower" "tostring" "toupper" "trim" "type" "upper")))

;; completion for keywords -- like company-keywords, but ignores case
(defun company-cypher (command &optional arg &rest _ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-cypher))
    (prefix (and (derived-mode-p 'cypher-mode)
                 (not (company-in-string-or-comment))
                 (or (company-grab-symbol) 'stop)))
    (candidates
     (let ((completion-ignore-case t))
       (all-completions arg nvp-cypher-completion-words)))))

(defun nvp-cypher-keywords-setup ()
  (make-local-variable 'company-backends)
  (push 'company-cypher company-backends)
  (setq-local company-dabbrev-code-ignore-case t)
  (setq-local company-dabbrev-ignore-case t))

;; -------------------------------------------------------------------
;;; Help

(defun nvp-cypher-help (arg)
  (interactive "P")
  (if arg
      (browse-url "https://neo4j.com/docs/cypher-refcard/current/")
    (let ((sym (downcase (thing-at-point 'symbol))))
      (if (and sym (string-match cypher-clauses sym))
          (browse-url
           (format "http://neo4j.com/docs/developer-manual/%s/cypher/clauses/%s/"
                   (nvp-neo4j-version) sym))
        (browse-url "https://neo4j.com/docs/cypher-refcard/current/")
        (message "%s not a cypher clause" sym)))))

(provide 'nvp-neo)
;;; nvp-neo.el ends here
