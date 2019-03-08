;;; nvp-org.el --- org helpers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-07 20:12:22>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/md-tools
;; Package-Requires: 
;; Created: 20 February 2019

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro)
  (nvp-local-vars))
(require 'org)
(require 'nvp)

(declare-function org-table-check-inside-data-field "org-table")
(declare-function org-table-end-of-field "org-table")
(declare-function org-table-beginning-of-field "org-table")
(declare-function org-babel-execute:dot "ob-dot")
(declare-function outline-show-subtree "outline")
(autoload 'nvp-list-split "nvp-util")

;; -------------------------------------------------------------------
;;; Utils

;; Note: this isn't used anywhere
;; Format alist DAT as an org table.  This alist assumes :head and
;; :rows lists. It splits :rows into number of sublists matching
;; number of colums (:head).
(defun nvp-table-alist-to-org (dat)
  (let* ((head (cdr (assoc-string "head" dat)))
         (rows (append (cdr (assoc-string "rows" dat)) ()))
         (cols (length head)))
    (append head (cons 'hline nil) (nvp-list-split rows cols) nil)))

;; walk org-tree
;; Modified https://gist.github.com/theodorewiles/cce2c170f8d4dfc60f06073cb73dfe10
(defun nvp-org-header-list (&optional header-re level buffer items)
  "Get the headers of an org buffer (default current buffer). Optionally,
narrows to headers matching HEADER-RE under nesting LEVEL (defaults all
headers). Returns plist list of headers with specified values in ITEMS when defined.
Defaults to header text, location, level, todo status. 
See `org-element-all-elements' for possible item types."
  (setq level (or level 0)) ;default all headers
  (save-restriction
    (with-current-buffer (or buffer (current-buffer))
      (goto-char (point-min))
      (and header-re
           (re-search-forward header-re nil nil 1)
           (org-narrow-to-element))
      (let ((tree (org-element-parse-buffer 'headline)))
        
        (list :buffer (current-buffer)
              :headers
              (nreverse
               (org-element-map tree 'headline
                 (lambda (el)
                   (when (< level (org-element-property :level el))
                     (or (and items
                              (cl-loop for sym in items
                                 nconc (list sym (org-element-property sym el))))
                         (list :raw-value (org-element-property :raw-value el) ;text
                               :begin (org-element-property :begin el) ;start
                               :end (org-element-property :end el)     ;end
                               :level (org-element-property :level el) ;depth
                               :todo-keyword (org-element-property :todo-keyword el))
                         ))))))))))

;; -------------------------------------------------------------------
;;; Org-clock

(autoload 'org-clock-history-push "org-clock")

(defun nvp-org-goto-clock ()
  (interactive)
  (find-file-other-window (expand-file-name "gtd.org" nvp/org))
  (goto-char (point-min))
  (search-forward "* Tasks" nil 'move)
  (outline-show-subtree))

;; construct list of tasks to choose from in clock history buffer
;; #<marker at 19296 in org-clock.el>
(defun nvp-org-clock-create (headers)
  "Create clock tasks for headers."
  (let ((buf (plist-get headers :buffer)))
    (cl-loop for h in (plist-get headers :headers)
       when (string= "TODO" (plist-get h :todo-keyword))
       do (org-clock-history-push (plist-get h :begin) buf))))

;; generate a list of TODO tasks nested under Tasks heading from file
;;;###autoload
(defun nvp-org-clock-add-tasks (&optional arg file-name)
  (interactive "P")
  (setq file-name (or file-name (expand-file-name "gtd.org" nvp/org)))
  (let ((buff (or (get-file-buffer file-name)
                  (find-file-noselect file-name))))
    (nvp-org-clock-create
     (nvp-org-header-list
      (if arg (read-string "Task group: ") "Tasks") 1 buff
      '(:raw-value :begin :todo-keyword)))))

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
       (format 
        (nvp-concat "%s [ label=\"%s\" shape=%s style=\"filled\" "
                    "fillcolor=\"%s\"]")
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

;; -------------------------------------------------------------------
;;; Commands

(defun nvp-org-mark-field ()
  (interactive)
  (require 'org-table)
  (org-table-check-inside-data-field)
  (org-table-end-of-field 1)
  (set-mark (point))
  (org-table-beginning-of-field 1))

;; go up element, or if at top-level back same level
(defun nvp-org-up-or-back-element ()
  (interactive)
  (condition-case nil
      (org-up-element)
    (error (org-backward-heading-same-level 1))))

;; go down element when possible, otherwise forward
(defun nvp-org-down-or-forward-element ()
  (interactive)
  (condition-case nil
      (org-down-element)
    (error (org-forward-heading-same-level 1))))

;; FIXME: don't skip over tags
;; ensure point is at end-of-line so text doesn't get carried to next todo
(define-advice org-insert-todo-heading (:before (&rest _args) "move-eol")
  (end-of-line))

;; capture
;;;###autoload
(defun nvp-org-capture (&optional goto keys)
  (interactive)
  (require 'nvp-vars)
  (org-capture goto keys))

;; -------------------------------------------------------------------
;;; Export

;; Adds a header, for org-themes?
(defun nvp-org-prep-header (str)
  (with-temp-buffer
    (insert str)
    (replace-regexp-in-string "#\\+HTML_HEAD:\\|# -\\*-.*$" "" (buffer-string))))

;; -------------------------------------------------------------------
;;; Toggle / insert

(defun nvp-org-toggle-parent-checkbox ()
  "Add checkbox to parent header if not there and update block statistics."
  (interactive)
  (when (not (eq 0 (org-outline-level)))
    (save-excursion
      (org-back-to-heading t)
      (end-of-line)
      (unless (looking-back "\\[[0-9]*/[0-9]*\\][ \t]*" (line-beginning-position))
        (delete-horizontal-space)
        (insert " [/]"))
      (org-update-checkbox-count))))

(provide 'nvp-org)
;;; nvp-org.el ends here
