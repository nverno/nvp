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

(defvar-local nvp-local-snippet-conditions ()
  "Directory local variable to set condition in snippet subdirectories.")
(put 'nvp-local-snippet-conditions 'safe-local-variable 'listp)


;;; Hooks

(nvp:decl nvp-yas-in-string nvp-yas-in-comment)

(define-advice yas-maybe-load-snippet-buffer
    (:around (orig-fn &rest args) "save-excursion")
  (save-excursion (apply orig-fn args)))

;; Return marker at end of snippet header.
(nvp:define-cache nvp-snippet-header-end ()
  :local t
  :cache nvp-snippet-header-end--cache
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "# --" nil t)
      (point-marker))))

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
  ;; With prefix dired the snippet directory
  (if do-dired (dired snippet-dir)
    (let ((yas-wrap-around-region nil)  ; don't insert selected twice
          (yas-selected-text text)
          (yas-indent-line 'fixed)
          (default-directory snippet-dir))
      (switch-to-buffer-other-window (generate-new-buffer "*snippet*"))
      (if (fboundp 'snippet-ts-mode)
          (snippet-ts-mode)
        (snippet-mode))
      (yas-minor-mode)
      (yas-expand-snippet default-new-snippet))))


;;; Indentation

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

(provide 'nvp-snippet)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-snippet.el ends here
