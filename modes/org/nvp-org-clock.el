;;; nvp-org-clock.el --- Org clock -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'org-clock)
(nvp:decls :p (org))

(autoload 'org-element-parse-buffer "org-element")


(defun nvp-org-goto-clock ()
  (interactive)
  (find-file-other-window (expand-file-name "gtd.org" nvp/org))
  (goto-char (point-min))
  (search-forward "* Tasks" nil 'move)
  (outline-show-subtree))

;; #<marker at 19296 in org-clock.el>
(defun nvp-org-clock-create (headers)
  "Create clock tasks for headers.
Construct list of tasks to choose from in clock history buffer."
  (let ((buf (plist-get headers :buffer)))
    (cl-loop for h in (plist-get headers :headers)
             when (string= "TODO" (plist-get h :todo-keyword))
             do (org-clock-history-push (plist-get h :begin) buf))))

;; Walk org-tree
;; Modified https://gist.github.com/theodorewiles/cce2c170f8d4dfc60f06073cb73dfe10
(defun nvp-org-header-list (&optional header-re level buffer items)
  "Get the headers of an org buffer (default current buffer). Optionally,
narrows to headers matching HEADER-RE under nesting LEVEL (defaults all
headers). Returns plist list of headers with specified values in ITEMS when
defined. Defaults to header text, location, level, todo status. 
See `org-element-all-elements' for possible item types."
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

;;;###autoload
(defun nvp-org-clock-add-tasks (&optional arg file-name)
  "Generate a list of TODO tasks nested under Tasks heading from FILE-NAME.
With prefix ARG, prompt for task group."
  (interactive "P")
  (setq file-name (or file-name (expand-file-name "gtd.org" nvp/org)))
  (let ((buff (or (get-file-buffer file-name)
                  (find-file-noselect file-name))))
    (nvp-org-clock-create
     (nvp-org-header-list
      (if arg (read-string "Task group: ") "Tasks") 1 buff
      '(:raw-value :begin :todo-keyword)))))


(provide 'nvp-org-clock)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-org-clock.el ends here
