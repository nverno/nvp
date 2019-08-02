;;; nvp-align.el --- alignment -*- lexical-binding: t; -*-

;;; Commentary:

;; Alignment:
;; - interactive align commands
;; - show rules applicable for mode
;; - additional alignment rules

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-results))
(require 'nvp)
(require 'nvp-results)
(require 'nvp-read)
(require 'align)
(nvp-decls)

;;;###autoload
(defun nvp-align (&optional arg beg end)
  "Align buffer region b/w BEG and END, or call `nvp-mark-defun' if nil. 
(4) prefix, align entire active region or buffer. 
(16) prefix, highlight changes that would occur."
  (interactive
   (cons (prefix-numeric-value current-prefix-arg)
         (nvp-tap-or-region 'bdwim (nvp-prefix 4 'buffer 'defun) :pulse t)))
  (if (eq arg 16) (call-interactively 'align-highlight-rule)
    (indent-region beg end)
    (align beg end)))

;; Align single end-of-line comments within marked regions. 
;; Doesn't align if double-quote is found before end-of-line. Not robust,
;; better to use `align-mode-rules-list' to account for comments/strings
;;;###autoload
(defun nvp-align-comments (beg end)
  (interactive "*r")
  (let ((start (regexp-quote (string-trim comment-start)))
        indent-tabs-mode align-to-tab-stop)
    (if (not (eq major-mode 'c-mode))
        (align-regexp beg end (concat "\\(\\s-+\\)" start "[^" start "\"][^\"\n]*")
                      nil 2)
      (align-regexp beg end (concat "\\(\\s-*\\)\\(?://\\|/\\*\\)")))))

;;;###autoload
(defun nvp-align-by-last-char (char &optional beg end)
  "Align BEG to END or bounds of paragraph by CHAR.
With prefix or if char is '\\', ensure CHAR is at the end of the line."
  (interactive (nvp-with-region beg end 'paragraph :pulse t
                 (list (nvp-input 'lcs) beg end)))
  (let ((re (concat "\\(\\s-+\\)" (regexp-quote char)
                    (if (or current-prefix-arg (string= char "\\")) "$" ""))))
    (align-regexp beg end re)))

;; Repeat alignment with respect to `REGEXP'. If `JUSTIFY-RIGHT'
;; is non-nil, justify to the right. If `AFTER', add whitespace to left
;; instead of right.
;;;###autoload
(defun nvp-align-repeat (start end regexp &optional justify-right after)
  (interactive "r\nsAlign regexp: ")
  (let ((complete-regexp (if after
                             (concat regexp "\\([ \t]*\\)")
                           (concat "\\([ \t]*\\)" regexp)))
        (group (if justify-right -1 1)))
    (align-regexp start end complete-regexp group 1 t)))

;;; FIXME: ignore commented regions
;; Align text as columns. Originally made for aligning abbrevs.
;; Text is treated as anything between '', "", or non-spaces.
;;;###autoload
(defun nvp-align-cols (start end)
  (interactive "r")
  (nvp-align-repeat start end "\\(\"[^\"]+\"\\|\'[^\']+'\\|[^ ]+\\)")
  (indent-region start end))

;; -------------------------------------------------------------------
;;; Alignment rules

;; `align-exclude-rules-list'
;; `align-rules-list'

;; Collect align rules from VAR for MODE
(defun nvp-align--mode-rules (var &optional mode)
  (or mode (setq mode major-mode))
  (--filter
   (--> (cdr (assoc 'modes (cddr it)))
        (memq mode (if (symbolp it) (symbol-value it) it)))
   var))

;;;###autoload
(defun nvp-align-show-rules (&optional mode)
  "Show align/exclude rules applicable to `major-mode' or MODE."
  (interactive (list (nvp-prefix 4 (nvp-read-mode) major-mode)))
  (let ((rules (nvp-align--mode-rules align-rules-list mode))
        (excludes (nvp-align--mode-rules align-exclude-rules-list mode)))
    (nvp-with-results-buffer nil
      (nvp-results-title (format "Align rules for %s" mode))
      (insert (pp-to-string rules))
      (terpri)
      (insert (pp-to-string excludes))
      (emacs-lisp-mode))))

(provide 'nvp-align)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-align.el ends here
