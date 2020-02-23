;;; nvp-org-dot.el --- tables / graphviz -*- lexical-binding: t; -*-

;;; Commentary:
;; - Stuff related to org-tables
;; - convert tables to graphviz
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-org)
(require 'org-table)
(declare-function org-babel-execute:dot "ob-dot")

;; Note: this isn't used anywhere
;; Format alist DAT as an org table.  This alist assumes :head and
;; :rows lists. It splits :rows into number of sublists matching
;; number of colums (:head).
(defun nvp-table-alist-to-org (dat)
  (let* ((head (cdr (assoc-string "head" dat)))
         (rows (append (cdr (assoc-string "rows" dat)) ()))
         (cols (length head)))
    (append head (cons 'hline nil) (nvp-list-split-into-sublists rows cols) nil)))

;; -------------------------------------------------------------------
;;; Commands

;;;###autoload
(defun nvp-org-mark-field ()
  (interactive)
  (org-table-check-inside-data-field)
  (org-table-end-of-field 1)
  (set-mark (point))
  (org-table-beginning-of-field 1))

;; https://www.reddit.com/r/emacs/comments/bc05il/extending_expandregion_to_mark_table_cell_in_org/
;;;###autoload
(defun nvp-er/mark-org-table-cell ()
  "Marks a table cell"
  (interactive)
  (search-backward "|")
  (set-mark (point))
  (search-forward "|" nil nil 2)
  (exchange-point-and-mark))

;; -------------------------------------------------------------------
;;; Graphviz

;; org-table column values
(defsubst nvp-org--column-values (col table)
  (cl-loop for row in table
     collect (nth col row)))

;; group column1 by column2 in table
;; predicate is used to order the result -- use 'string< if grouping by
;; a character column, for example
(defun nvp-org-group-by (col1 col2 table &optional predicate)
  (let ((vals (sort (cl-remove-duplicates
                     (nvp-org--column-values col2 table))
                    (or predicate '<))))
    (cl-loop for val in vals
       collect (cons val (cl-loop for row in table
                            when (eq val (nth col2 row))
                            collect (nth col1 row))))))

;; create a graphvis declaration such that nodes of the same rank
;; have invisible edges to those of higher rank, creating an ordering
(defun nvp-org-node-ranks (table &optional vrank)
  (let ((ranks (nvp-org-group-by 0 (if vrank 5 4) table)))
    (concat
     (mapconcat
      (lambda (x)
        (format "{rank=same %s}" (mapconcat 'identity (cdr x) " ")))
      ranks
      " -> ")
     "[style=invis]")))

;; Convert org-table to dot graph
(defun nvp-org-dotify (nodes graph &rest params)
  (org-babel-execute:dot
   (concat
    ;; remove comment characters '//' for horizontal layout, add vertical layout
    "digraph {\nrankdir=LR;\n"
    (nvp-org-node-ranks nodes)        ;horizontal ranks
    ;; nodes
    (mapconcat
     (lambda (x)
       (format "%s [ label=\"%s\" shape=%s style=\"filled\" fillcolor=\"%s\"]"
               (car x)
               (nth 1 x)
               (if (string= "" (nth 2 x)) "box" (nth 2 x))
               (if (string= "" (nth 3 x)) "none" (nth 3 x))
               ;; (nth 4 x)
               ))                     ;ranked left->right
     nodes "\n")
    "\n"
    ;; edges
    (mapconcat
     (lambda (x)
       (format "%s -> %s [label=\"%s\", style=\"%s\", weight=\"%s\"]"
               (car x) (nth 1 x) (nth 2 x) (nth 3 x) (nth 4 x)))
     graph "\n")
    "}\n")
   params)
  (format "[[file:%s]]" (cdr (assq :file params))))


(provide 'nvp-org-dot)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-org-dot.el ends here
