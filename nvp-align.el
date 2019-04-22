;;; nvp-align.el --- alignment -*- lexical-binding: t; -*-

;;; Commentary:

;; Alignment:
;; - interactive align commands
;; - TODO: describe rules applicable for mode
;; - additional alignment rules

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro)
  (require 'nvp-results))
(nvp-decls)
(require 'nvp)
(require 'nvp-read)
(require 'align)

;;;###autoload
(defun nvp-align (&optional arg beg end)
  "Align buffer region b/w BEG and END, or call `nvp-mark-defun' if nil. 
(4) prefix, align entire active region or buffer. 
(16) prefix, highlight changes that would occur."
  (interactive
   (cons (prefix-numeric-value current-prefix-arg)
         (nvp-tap-or-region 'bdwim (nvp-prefix 4 'buffer 'defun) :pulse t)))
  (if (eq arg 16) (call-interactively 'align-highlight-rule)
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
                 (list (nvp-input 'lce) beg end)))
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

;; (defun nvp-align-mode-rules (&optional mode)
;;   "Display alignment rules applicable to MODE (default `major-mode').
;; With prefix, prompt for mode."
;;   (interactive (list (nvp-prefix 4 (nvp-read-mode) major-mode)))
;;   (nvp-with-results-buffer (help-buffer)
;;     ))

(provide 'nvp-align)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-align.el ends here
