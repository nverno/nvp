;;; nvp-org.el --- org helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'org)
(require 'nvp)
(nvp-decls :f (outline-show-subtree
               org-element-parse-buffer org-element-map org-element-property
               org-tempo-setup))

;; -------------------------------------------------------------------
;;; Utils

;; walk org-tree
;; Modified https://gist.github.com/theodorewiles/cce2c170f8d4dfc60f06073cb73dfe10
(defun nvp-org-header-list (&optional header-re level buffer items)
  "Get the headers of an org buffer (default current buffer). Optionally,
narrows to headers matching HEADER-RE under nesting LEVEL (defaults all
headers). Returns plist list of headers with specified values in ITEMS when defined.
Defaults to header text, location, level, todo status. 
See `org-element-all-elements' for possible item types."
  (require 'org-element)
  (setq level (or level 0)) ;default all headers
  (save-restriction
    (with-current-buffer (or buffer (current-buffer))
      (goto-char (point-min))
      (and header-re
           (re-search-forward header-re nil nil 1)
           (org-narrow-to-element))
      (let ((tree (org-element-parse-buffer 'headline)))
        (list
         :buffer (current-buffer)
         :headers
         (nreverse
          (org-element-map tree 'headline
            (lambda (el)
              (when (< level (org-element-property :level el))
                (or (and items
                         (cl-loop for sym in items
                            nconc (list sym (org-element-property sym el))))
                    (list
                     :raw-value (org-element-property :raw-value el)  ;text
                     :begin (org-element-property :begin el)          ;start
                     :end (org-element-property :end el)              ;end
                     :level (org-element-property :level el)          ;depth
                     :todo-keyword (org-element-property :todo-keyword el))))))))))))

;; -------------------------------------------------------------------
;;; Commands

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

;;; XXX: it would be nice to get completion like the old hydra
(defun nvp-org-src-maybe ()
  "When '<' is entered at the beginning of a line, load org-tempo.el."
  (interactive)
  (when (and (looking-back "^" (line-beginning-position))
             (require 'org-tempo))    ; load shortcuts
    (org-tempo-setup)
    (define-key org-mode-map "<" #'self-insert-command))
  (self-insert-command 1))

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
;;; Export

;; Adds a header, for org-themes?
(defun nvp-org-prep-header (str)
  (with-temp-buffer
    (insert str)
    (replace-regexp-in-string "#\\+HTML_HEAD:\\|# -\\*-.*$" "" (buffer-string))))


(provide 'nvp-org)
;;; nvp-org.el ends here
