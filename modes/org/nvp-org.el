;;; nvp-org.el --- org helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'org)
(require 'nvp)
(nvp:req 'nvp-org 'subrs)

(nvp:decls
 :p (outline org)
 :f ( org-element-type org-element--property org-tempo-setup org-element-map
      org-element-parse-buffer))

(autoload 'nvp-parse-current-function "nvp-parse")

;;;###autoload
(defun nvp-org-tags-view (&optional directory)
  "Display org tags with `org-tags-view'.
When call from an `org-mode' buffer with \\[universal-argument], limit tags
to those in current buffer.
Elsewhere, or with prefix >= 4, prompt for DIRECTORY to search within."
  (interactive
   (list (let ((raw (prefix-numeric-value current-prefix-arg)))
           (cond ((and (= raw 4)
                       (derived-mode-p 'org-mode))
                  (buffer-file-name))
                 ((>= raw 4) (completing-read-multiple
                              "Directory: " #'completion-file-name-table))
                 (t nil)))))
  (let ((org-agenda-files (if directory (nvp:as-list directory)
                            org-agenda-files)))
    (nvp:prefix-shift -1)
    (call-interactively #'org-tags-view)))

;; Walk org-tree
;; Modified https://gist.github.com/theodorewiles/cce2c170f8d4dfc60f06073cb73dfe10
(defun nvp-org-header-list (&optional header-re level buffer items)
  "Get the headers of an org buffer (default current buffer). Optionally,
narrows to headers matching HEADER-RE under nesting LEVEL (defaults all
headers). Returns plist list of headers with specified values in ITEMS when
defined. Defaults to header text, location, level, todo status. 
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
;;; Links

;;;###autoload
(defun nvp-org-links (headline-re &optional buffer-or-file type)
  "Gather links of TYPE (or \"https\") nested under sections matching 
HEADLINE-RE in BUFFER if non-nil or current buffer . 
Return cons of \\='(name                           . raw-link)."
  (let ((buf (if buffer-or-file
                 (or (get-buffer buffer-or-file)
                     (find-file-noselect buffer-or-file))
               (current-buffer)))
        res)
    (with-current-buffer buf
      (nvp:with-org-sections headline-re
        :types (link)
        (when (or (null type)
                  (string-match-p type (org-element-property :type it)))
          (push (list
                 (concat
                  (and (length> prefix 0) (concat prefix "/"))
                  (when-let* ((beg (org-element-property :contents-begin it))
                              (end (org-element-property :contents-end it)))
                    (and beg end (buffer-substring-no-properties beg end))))
                 'org-link it)
                res))))
    (nreverse res)))

;; Link format:
;; nvp:library ( '?' section-or-def ( '&' 'type=' (v|f|s) )? )?
(org-link-set-parameters "nvp"
  :store  #'nvp-org-nvp-store-link
  :follow #'nvp-org-nvp-open
  :export #'nvp-org-nvp-export)
(put 'org-link-set-parameters 'lisp-indent-function 1)

(defvar nvp-org-nvp-re
  "\\([^?]+\\)\\(?:[?]\\([^&]+\\)\\)?\\(?:&type=\\([vfs]\\)\\)?"
  "Match parts of link query.")

(defsubst nvp-org--nvp-parse (query)
  (when (string-match nvp-org-nvp-re query)
    (list (match-string 1 query)
          (match-string 2 query)
          (pcase (match-string 3 query)
            ("f" 'function)
            ("s" 'section)
            ("v" 'variable)
            (_ nil)))))

(defun nvp-org-nvp-open (query)
  "Visit nvp FILE-SECTION and goto SECTION if non-nil."
  (-let (((file sec-or-def type) (nvp-org--nvp-parse query)))
    (--if-let (nvp:locate-library file)
        (with-current-buffer (find-file-noselect it)
          (let ((cur (point)) pt)
            (when sec-or-def
              (goto-char (point-min))
              (let* ((prefix-re (if (or (not type) (eq type 'section))
                                    (concat (nvp:heading-create-re) "*")
                                  "^\\s-*(def.*"))
                     (case-fold-search t))
                (when (re-search-forward (concat prefix-re sec-or-def) nil t)
                  (setq pt (nvp:point 'bol)))))
            (pop-to-buffer (current-buffer))
            (unless (or (null pt) (eq pt cur))
              (push-mark))
            (and pt (goto-char pt))
            (recenter-top-bottom)))
      (user-error "No library found for \"%s\"" file))))

(defun nvp-org-nvp-export (file-section desc backend _)
  (-let* (((file _section) (split-string file-section "?"))
          (lib (or (nvp:locate-library file) file)))
    (pcase backend
      (`texinfo (format "@uref{%s,%s}" lib desc))
      (_ lib))))

(defun nvp-org-nvp-store-link (arg &optional _interactive?)
  "Store org \\='nvp link."
  (when-let ((lib (and (eq major-mode 'emacs-lisp-mode)
                       (file-name-base (buffer-file-name)))))
    (let* ((prompt (> (prefix-numeric-value arg) 1))
           (symt (cond ((nvp:ppss 'cmt) (cons 's nil))
                       (t (cons 'f (nvp-parse-current-function))))))
      (when (or prompt (null (cdr symt)))
        (let* ((typ (nvp:read-char-case "Type: " 'verbose
                      (?s "[s]ection" 's)
                      (?f "[f]unction" 'f)
                      (?v "[v]ariable" 'v)))
               (def (and (eq (car symt) typ) (cdr symt)))
               (sym (read-string "Thing: " def nil def)))
          (setq symt (cons typ sym))))
      (and symt
           (org-link-store-props
            :type "nvp"
            :link (format "nvp:%s?%s&type=%s" lib (cdr symt) (car symt))
            :description (format "%s" (cdr symt)))))))

;; -------------------------------------------------------------------
;;; Commands

(defun nvp-org-forward-element ()
  (interactive)
  (nvp:push-mark 'nvp-org-forward-element)
  (cond ((org-at-item-p) (condition-case nil
                             (org-next-item)
                           (error (org-forward-element))))
        (t (org-forward-element))))

(defun nvp-org-backward-element ()
  (interactive)
  (let ((pushed (not (eq t (nvp:push-mark 'nvp-org-backward-element)))))
    (condition-case err
        (org-backward-element)
      (error (and pushed (pop-mark))
             (error (error-message-string err))))))

(nvp:def-keymap nvp-repeat-org-move
  :repeat (:enter (nvp-org-forward-element org-backward-element))
  "TAB" #'org-cycle
  "n"   #'nvp-org-forward-element
  "j"   #'nvp-org-forward-element
  "p"   #'nvp-org-backward-element
  "k"   #'nvp-org-backward-element
  "l"   #'org-down-element
  "h"   #'org-up-element)

;; Ensure point is at end-of-line so text doesn't get carried to next todo
(define-advice org-insert-todo-heading (:before (&rest _args) "move-eol")
  (end-of-line))

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

;; XXX: it would be nice to get completion like the old hydra
(defun nvp-org-src-maybe ()
  "When '<' is entered at the beginning of a line, load org-tempo.el."
  (interactive)
  (when (and (looking-back "^\\s-*" (line-beginning-position))
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
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-org.el ends here
