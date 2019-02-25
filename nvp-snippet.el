;;; nvp-snippet.el --- snippet mode -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-24 19:30:50>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created:  7 February 2019

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'nvp-macro)
  (nvp-local-vars))
(require 'nvp)
(require 'yasnippet)
(declare-function nvp-read-mode "nvp-read")

;; -------------------------------------------------------------------
;;; Snippet mode
;; #<marker at 172452 in yasnippet.el>

;; "'" should be prefix to enable quote wrapping etc.
(modify-syntax-entry ?$ "'" snippet-mode-syntax-table)
(modify-syntax-entry ?` "(`" snippet-mode-syntax-table)
(modify-syntax-entry ?` ")`" snippet-mode-syntax-table)

(nvp-function-with-cache nvp-snippet-header-end ()
  "Return marker at end of snippet header."
  :local t
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "# --")
      (point-marker))))

(defsubst nvp-snippet-header-p (&optional pnt)
  (< (or pnt (point)) (marker-position (nvp-snippet-header-end))))

;; (defsubst nvp-snippet-code-p (&optional pnt)
;;   ())

;; -------------------------------------------------------------------
;;; Utils

(defun nvp-snippet-add-field (field value)
  "Add FIELD with VALUE unless FIELD is already defined."
  (save-excursion
    (goto-char (point-min))
    (let ((end (nvp-snippet-header-end)))
      (condition-case nil
          (when (re-search-forward (concat "^#\\s-*" field ":") end)
            (message "%s already defined: %S" field
                     (buffer-substring-no-properties (point) (point-at-eol))))
        (error
         (goto-char end)
         (beginning-of-line)
         (insert (format "# %s: %S\n" field value)))))))

;; -------------------------------------------------------------------
;;; Hooks
(nvp-declare "" nvp-yas-inside-string nvp-yas-inside-comment)

(defun nvp-snippet-before-save-hook ()
  "Add conditions based on directory names."
  (pcase (nvp-dfn)
    (`"doc" (nvp-snippet-add-field "condition" '(nvp-yas-inside-string)))
    (`"comments" (nvp-snippet-add-field "condition" '(nvp-yas-inside-comment)))
    (_)))

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
   (let ((mode-name (if (equal current-prefix-arg '(64)) (nvp-read-mode)
                      (symbol-name major-mode))))
     (list mode-name
           (if (or (equal current-prefix-arg '(16)) (not nvp-snippet-dir))
               (expand-file-name mode-name nvp/snippet)
             nvp-snippet-dir)
           (and (equal current-prefix-arg '(4)) 'do-dired)
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

;; de/in-crement snippet expansion numbers in selected region
(defun nvp-snippet-increment-count (bounds)
  (interactive (list (nvp-region-or-batp)))
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

(provide 'nvp-snippet)
;;; nvp-snippet.el ends here
