;;; nvp-snippet.el --- snippet mode -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'yasnippet)
(nvp-decl "nvp-read" nvp-read-mode nvp-read-mode-var)

(defvar nvp-snippet-default-conditions
  '(("comment" (nvp-yas-in-comment))
    ("cookie"  (bound-and-true-p cookie))
    ("doc"     (nvp-yas-in-comment)))
  "Directory names to default conditions (eg. :condition key functions).")

;; local variables to update fields in snippet subdirs
(defvar-local nvp-snippet-local-conditions ()
  "Directory local variable to set condition in snippet subdirectories.")
(put 'nvp-snippet-local-conditions 'safe-local-variable 'listp)

;; -------------------------------------------------------------------
;;; Hooks
(nvp-decl nvp-yas-in-string nvp-yas-in-comment)

(define-advice yas-maybe-load-snippet-buffer
    (:around (orig-fn &rest args) "save-excursion")
  (save-excursion
    (apply orig-fn args)))

(defun nvp-snippet-save-hook ()
  "Add conditions based on directory names.
When part of `before-save-hook', won't add condition on initial save."
  (unless buffer-file-name
    (let ((before-save-hook ()))
      (save-buffer)))
  (cl-flet ((add-condition
             (pred)
             (remove-hook 'before-save-hook #'nvp-snippet-save-hook t)
             (nvp-snippet-add-field "condition" pred)))
    (let ((dir (ignore-errors (or (nvp-path 'ds) (nvp-path 'dn)))))
      (when-let*
          ((test
            (assoc-string dir (or (bound-and-true-p nvp-snippet-local-conditions)
                                  nvp-snippet-default-conditions))))
        (add-condition (cadr test))))))

(defun nvp-snippet-add-field (field value)
  "Add FIELD with VALUE unless FIELD is already defined."
  (save-mark-and-excursion
    (goto-char (point-min))
    (let ((end (nvp-snippet-header-end)))
      (if (re-search-forward (concat "^#\\s-*" field ":") end t)
          (message "%s already defined: %S" field
                   (buffer-substring (point) (point-at-eol)))
        (goto-char end)
        (beginning-of-line)
        (insert (format "# %s: %S\n" field value))))))

;; ------------------------------------------------------------
;;; Jump to new snippet

;; #<marker at 95724 in yasnippet.el>
;;;###autoload
(defun nvp-jump-to-new-snippet (mode snippet-dir &optional do-dired text
                                     default-new-snippet)
  "Jump to a new snippet for MODE in snippet SNIPPET-DIR (creating if necessary).
 If DO-DIRED is non-nil, `dired' that directory instead of creating snippet.
If TEXT is non-nil use as `yas-selected-text'.
DEFAULT-NEW-SNIPPET is default snippet template to use if non-nil."
  (interactive
   (let* ((mode-name (nvp-prefix 16 (nvp-read-mode) (symbol-name major-mode)))
          (snippet-dir
           (if (not nvp-mode-snippet-dir)
               (expand-file-name mode-name nvp/snippet)
             (nvp-read-mode-var "snippets" (file-name-base nvp-mode-snippet-dir)))))
     (list mode-name
           snippet-dir
           (nvp-prefix 4 'do-dired)
           (or yas-selected-text
               (and (region-active-p)
                    (buffer-substring-no-properties
                     (region-beginning) (region-end)))))))
  (and (symbolp mode) (setq mode (symbol-name mode)))
  (or default-new-snippet (setq default-new-snippet yas-new-snippet-default))
  (unless (file-exists-p snippet-dir)
    (make-directory snippet-dir))
  ;; with prefix dired the snippet directory
  (if do-dired (dired snippet-dir)
    (let ((yas-wrap-around-region nil)  ;don't insert selected twice
          (yas-selected-text text)
          (default-directory snippet-dir))
      (switch-to-buffer-other-window (generate-new-buffer "*snippet*"))
      (snippet-mode)
      (yas-minor-mode)
      (yas-expand-snippet default-new-snippet))))

;; -------------------------------------------------------------------
;;; Commands

(defun nvp-snippet-toggle-indent (&optional beg end)
  "Switch b/w lisp indentation and relative indentation."
  (interactive)
  (let* ((iline (if (eq indent-line-function 'lisp-indent-line)
                    'indent-relative 'lisp-indent-line))
         (iregion (if (eq indent-region-function 'lisp-indent-region)
                      'indent-region-line-by-line 'lisp-indent-region)))
    (if (region-active-p)
        (setq beg (or beg (region-beginning))
              end (or end (region-end)))
      (setq beg (point-min) end (point-max)))
    (setq-local indent-line-function iline)
    (setq-local indent-region-function iregion)
    (indent-region beg end)))

(defun nvp-snippet-indent-line ()
  (interactive)
  (condition-case nil
      (funcall-interactively indent-line-function)
    (error (indent-relative))))

;; de/in-crement snippet expansion numbers in selected region
(defun nvp-snippet-increment-count (bounds)
  (interactive (list (nvp-tap 'bdwim)))
  (cl-destructuring-bind (beg . end) bounds
    (goto-char beg)
    (while (re-search-forward "\$\{?\\([[:digit:]]\\)" end 'move)
      (replace-match (number-to-string
                      (+ (if current-prefix-arg -1 1)
                         (string-to-number (match-string 1))))
                     nil nil nil 1))))

;;; FIXME:
(defun nvp-snippet-help-at-point ()
  (interactive)
  (browse-url "https://joaotavora.github.io/yasnippet/snippet-expansion.html"))

;; -------------------------------------------------------------------
;;; Snippet mode
;; #<marker at 172452 in yasnippet.el>

;; stuff to be evaluated once when file is loaded
;; "'" should be prefix to enable quote wrapping etc.
(modify-syntax-entry ?$ "'" snippet-mode-syntax-table)
;; (modify-syntax-entry ?\" "_" snippet-mode-syntax-table)
;; (modify-syntax-entry ?` ")`" snippet-mode-syntax-table)
;; (modify-syntax-entry ?` "(`" snippet-mode-syntax-table)

(nvp-define-cache nvp-snippet-header-end ()
  "Return marker at end of snippet header."
  :local t
  :cache nvp-snippet-header-end--cache
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "# --")
      (point-marker))))

;; non-nil if point is in header portion of snippet
(defsubst nvp-snippet-header-p (&optional pnt)
  (< (or pnt (point)) (marker-position (nvp-snippet-header-end))))

;; non-nil if point is in an elisp code segment
;; (defsubst nvp-snippet-code-p (&optional pnt)
;;   ())

(defvar nvp-snippet--field-vars
  (eval-when-compile
    (append
     '("-*- mode: snippet -*-")
     (mapcar
      #'(lambda (x) (concat x ": "))
      '("key" "name" "condition" "expand-env" "uuid" "group" "type")))))

(defvar nvp-snippet--env-vars
  '("((yas-indent-line 'fixed))" "((yas-wrap-around-region 'nil))"))

;; completion at point, use yas headers in header section, elisp capf otherwise
(defun nvp-snippet-capf ()
  (if (nvp-snippet-header-p)
      (save-excursion
        (let* ((end (point))
               (beg (progn (skip-syntax-backward "w_") (point)))
               (field (progn
                        (forward-line 0)
                        (and (looking-at "^#\\s-*\\([^: \n]+\\)\\s-*:?")
                             (match-string-no-properties 1))))
               (colon-p (and field (eq ?: (char-before (match-end 0))))))
          (goto-char beg)
          (skip-chars-backward " ")
          (cond
           ((eq (char-before) ?#)
            (list beg end nvp-snippet--field-vars))
           ((and colon-p (not (eq end beg)))
            (pcase field
              (`"expand-env" (list beg end nvp-snippet--env-vars))
              (`"type" (list beg end '("snippet" "command")))))
           ((not colon-p) (list beg end nvp-snippet--field-vars)))))
    (elisp-completion-at-point)))

(provide 'nvp-snippet)
;;; nvp-snippet.el ends here
