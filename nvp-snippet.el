;;; nvp-snippet.el --- snippet mode -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'yasnippet)
(nvp:decls)
(nvp:auto "f" f-parent)


(defvar nvp-snippet-default-conditions
  '(("comment" (nvp-yas-in-comment))
    ("cookie"  (bound-and-true-p cookie))
    ("doc"     (nvp-yas-in-doc)))
  "Directory names to default conditions (eg. :condition key functions).")

;; local variables to update fields in snippet subdirs
(defvar-local nvp-local-snippet-conditions ()
  "Directory local variable to set condition in snippet subdirectories.")
(put 'nvp-local-snippet-conditions 'safe-local-variable 'listp)


;;; Hooks
(nvp:decl nvp-yas-in-string nvp-yas-in-comment)

(define-advice yas-maybe-load-snippet-buffer
    (:around (orig-fn &rest args) "save-excursion")
  (save-excursion
    (apply orig-fn args)))

;; Return marker at end of snippet header.
(nvp:define-cache nvp-snippet-header-end ()
  :local t
  :cache nvp-snippet-header-end--cache
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "# --" nil t)
      (point-marker))))

;; non-nil if point is in header portion of snippet
;; (defsubst nvp-snippet-header-p (&optional pnt)
;;   (and-let* ((pos (nvp-snippet-header-end)))
;;     (< (or pnt (point)) (marker-position pos))))

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
    (let ((dir (ignore-errors (or (nvp:path 'ds) (nvp:path 'dn)))))
      (hack-local-variables)
      (when-let*
          ((test (or (bound-and-true-p nvp-local-snippet-conditions)
                     (-some-> (assoc-string dir nvp-snippet-default-conditions)
                       (cadr)))))
        (add-condition test)))))

(defun nvp-snippet-add-field (field value)
  "Add FIELD with VALUE unless FIELD is already defined."
  (save-mark-and-excursion
    (goto-char (point-min))
    (when-let ((end (nvp-snippet-header-end)))
      (if (re-search-forward (concat "^#\\s-*" field ":") end t)
          (message "%s already defined: %S" field
                   (buffer-substring (point) (line-end-position)))
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
   (let* ((mode-name (nvp:prefix 16 (nvp-read-mode)
                       (symbol-name (or nvp-mode-name major-mode))))
          (snippet-dir (or nvp-mode-snippet-dir (expand-file-name mode-name nvp/snippet))))
     ;; (if (not nvp-mode-snippet-dir)
     ;;     (expand-file-name mode-name nvp/snippet)
     ;;   (let ((dir (nvp-read-mode-var "snippets" mode-name)))
     ;;     (if nvp-mode-name
     ;;         (expand-file-name (nvp:as-string nvp-mode-name) (f-parent dir))
     ;;       dir))
     ;;   )
     (list mode-name
           snippet-dir
           (nvp:prefix 4 'do-dired)
           (or yas-selected-text
               (and (region-active-p)
                    (buffer-substring-no-properties
                     (region-beginning) (region-end)))))))
  (setq mode (nvp:as-string mode))
  (nvp:defq default-new-snippet yas-new-snippet-default)
  (unless (file-exists-p snippet-dir)
    (make-directory snippet-dir))
  ;; with prefix dired the snippet directory
  (if do-dired (dired snippet-dir)
    (let ((yas-wrap-around-region nil)  ;don't insert selected twice
          (yas-selected-text text)
          (yas-indent-line 'fixed)
          (default-directory snippet-dir))
      (switch-to-buffer-other-window (generate-new-buffer "*snippet*"))
      (if (fboundp 'snippet-ts-mode)
          (snippet-ts-mode)
        (snippet-mode))
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
  (let ((indent-line-function
         (if (eq last-command this-command) #'indent-relative
           #'indent-to-left-margin)))
    (funcall-interactively #'indent-for-tab-command)))


;;; XXX(5/16/24): rest is obsolete in `snippet-ts-mode'
;; (defvar-keymap nvp-repeat-snippet-inc-map
;;   :repeat t
;;   "=" #'nvp-snippet-increment-count
;;   "+" #'nvp-snippet-increment-count
;;   "-" #'nvp-snippet-decrement-count)
;; (put 'nvp-snippet-increment-count 'repeat-check-key 'no)

;; (defun nvp-snippet-increment-count (beg end &optional inc)
;;   "De/increment snippet expansion numbers in region BEG to END."
;;   (interactive (nvp:repeat-args (beg end inc)
;;                  (append (nvp:tap-or-region 'bdwim 'sexp :pulse t)
;;                          (list (if current-prefix-arg -1 1)))))
;;   (or inc (setq inc 1))
;;   (nvp-regex-map-across-matches
;;    (lambda (ms) (replace-match (number-to-string (+ inc (string-to-number ms)))
;;                                nil nil nil 1))
;;    "\$\{?\\([[:digit:]]\\)" (cons beg end) 1))

;; (defun nvp-snippet-decrement-count (beg end)
;;   (interactive (nvp:repeat-args (beg end)
;;                  (nvp:tap-or-region 'bdwim 'sexp :pulse t)))
;;   (setq this-command 'nvp-snippet-increment-count)
;;   (nvp-snippet-increment-count beg end -1))

;;; Snippet mode

;; stuff to be evaluated once when file is loaded
;; "'" should be prefix to enable quote wrapping etc.
;; (modify-syntax-entry ?$ "'" snippet-mode-syntax-table)
;; (modify-syntax-entry ?\" "_" snippet-mode-syntax-table)
;; (modify-syntax-entry ?` ")`" snippet-mode-syntax-table)
;; (modify-syntax-entry ?` "(`" snippet-mode-syntax-table)
;; (defvar nvp-snippet--xref-syntax-table
;;   (let ((table (copy-syntax-table snippet-mode-syntax-table)))
;;     (modify-syntax-entry ?$ "_" table)
;;     (modify-syntax-entry ?\? "_" table)
;;     (modify-syntax-entry ?~ "_" table)
;;     (modify-syntax-entry ?! "_" table)
;;     (modify-syntax-entry ?= "_" table)
;;     table))

;; (defun nvp-snippet-xref-find-definitions ()
;;   (interactive)
;;   (with-syntax-table nvp-snippet--xref-syntax-table
;;     (let* ((xref-backend-functions '(elisp--xref-backend t))
;;            (thing (xref-backend-identifier-at-point 'elisp)))
;;       (xref-find-definitions thing))))

;; non-nil if point is in an elisp code segment
;; (defsubst nvp-snippet-code-p (&optional pnt)
;;   ())

;; (defconst nvp-snippet--field-vars
;;   (cons
;;    "-*- mode: snippet -*-"
;;    (--map (concat it ": ")
;;           '("key" "name" "condition" "expand-env" "uuid" "group" "type"))))

;; (defvar nvp-snippet--env-vars
;;   '("((yas-indent-line 'fixed))" "((yas-wrap-around-region 'nil))"))

;; completion at point, use yas headers in header section, elisp capf otherwise
;; (defun nvp-snippet-completion-at-point ()
;;   (if (nvp-snippet-header-p)
;;       (save-excursion
;;         (let* ((end (point))
;;                (beg (progn (skip-syntax-backward "w_") (point)))
;;                (field (progn
;;                         (forward-line 0)
;;                         (and (looking-at "^#\\s-*\\([^: \n]+\\)\\s-*:?")
;;                              (match-string-no-properties 1))))
;;                (colon-p (and field (eq ?: (char-before (match-end 0))))))
;;           (goto-char beg)
;;           (skip-chars-backward " ")
;;           (cond
;;            ((eq (char-before) ?#)
;;             (list beg end nvp-snippet--field-vars))
;;            (colon-p ;(not (eq end beg))
;;             (pcase field
;;               (`"expand-env" (list beg end nvp-snippet--env-vars))
;;               (`"type" (list beg end '("snippet" "command")))))
;;            ((not colon-p) (list beg end nvp-snippet--field-vars)))))
;;     (elisp-completion-at-point)))

(provide 'nvp-snippet)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-snippet.el ends here
