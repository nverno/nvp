;;; nvp-edit.el --- editing autoloads -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-22 19:33:39>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 24 November 2016

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'smartparens)
  (require 'cl-lib)
  (defvar sort-fold-case)
  (defvar align-to-tab-stop))

(autoload 'sp-wrap-with-pair "smartparens")

;; -------------------------------------------------------------------
;;; Sort

;; list syntax
;; #<marker at 7784 in thingatpt.el.gz>
(defmacro nvp-sort-with-defaults (start end &rest body)
  "Sort region between START and END by BODY, using defaults and indent region \
afterward."
  (declare (indent defun) (debug (sexp sexp &rest form)))
  `(let ((sort-fold-case t))
     (save-excursion
       (save-match-data
         (unwind-protect
             ,@body
           (indent-region ,start ,end))))))

;;;###autoload
(defun nvp-sort-lines-first-word (start end &optional reverse)
  "Sort lines b/w START and END by first alphanumeric characters.
With prefix sort in REVERSE."
  (interactive "r\nP")
  (nvp-sort-with-defaults start end
    (sort-regexp-fields reverse "^.*$" "\\([[:alnum:]]+\\)" start end)))

;;;###autoload
(defun nvp-sort-list (&optional start end reverse)
  "Sort list region in list at point or b/w START and END by words/symbols.
With prefix sort in REVERSE."
  (interactive "r\nP")
  (nvp-within-bounds-of-thing-or-region 'list start end
    (nvp-sort-with-defaults start end
      (sort-regexp-fields reverse (nvp-concat "\\(?:"
                                              "\\s\"\\S\"*\\s\"" ;quoted
                                              "\\|\\sw+\\|\\s_+" ;word/symbol
                                              "\\)")
                          "\\(\\sw\\|\\s_\\)+" start end))))

;;;###autoload
(defun nvp-sort-alist (&optional start end reverse)
  "Sort alist by car of each element in list at point or b/w START and END."
  (interactive "r\nP")
  (nvp-within-bounds-of-thing-or-region 'list start end
    (nvp-sort-with-defaults start end
      ;; skip to first inner '(' eg. if this is a macro body
      (goto-char start)
      ;; sort between starting and ending cells
      (let ((epos (save-excursion (forward-list) (1- (point))))
            (bpos (progn
                    (forward-char 1)
                    (skip-syntax-forward "^(")
                    (point))))
        (sort-regexp-fields
         reverse "\\s-*([^\)]*)\\(?:[^\(]*$\\)?" "\\([[:alnum:]]\\)" bpos epos)))))

;;;###autoload
(defun nvp-sort-words (start end &optional reverse)
  (interactive "r\nP")
  (nvp-sort-with-defaults start end
   (sort-regexp-fields reverse "[^ \t\n]+" "\\&" start end)))

;;;###autoload
(defun nvp-sort-symbols (start end &optional reverse)
  (interactive "r\nP")
  (nvp-sort-with-defaults start end
    (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" start end)))

;; -------------------------------------------------------------------
;;; Fill 

;; Convert multiple line paragraph into one line.
;;;###autoload
(defun nvp-unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (call-interactively 'fill-paragraph)))

;; Remove newlines in list, leaving single spaces.
;;;###autoload
(defun nvp-unfill-list (begin end &optional regexp)
  (interactive "r")
  (setq regexp
        (if current-prefix-arg
            (read-regexp "Unfill regexp: " "[ \n\t\r]+")
          "[ \n\t\r]+"))
  (save-excursion
    (narrow-to-region begin end)
    (goto-char begin)
    (while (search-forward-regexp regexp end t)
      (replace-match " " nil t))
    (widen)
    (let ((fill-column 75))
      (fill-paragraph))))

;;; FIXME:
;;;###autoload
(defun nvp-fill-paragraph-toggle ()
  (interactive)
  (let (deactivate-mark
        (fill-column (nvp-toggled-if fill-column most-positive-fixnum)))
    (call-interactively 'fill-paragraph)))

;; -------------------------------------------------------------------
;;; Align 

;; ensure spaces when aligning
;;;###autoload(advice-add 'align-regexp :around #'align-regexp@no-tabs)
(define-advice align-regexp (:around (old-fn &rest args) "no-tabs")
  (let ((indent-tabs-mode nil))
    (apply old-fn args)))

;; Repeat alignment with respect to `REGEXP'. If `JUSTIFY-RIGHT'
;; is non-nil, justify to the right. If `AFTER', add whitespace to left
;; instead of right.
;;;###autoload
(defun nvp-align-repeat (start end regexp
                               &optional justify-right after)
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

;;;###autoload (autoload 'nvp-align-backslash "nvp-edit")
(nvp-align-fn nvp-align-backslash
  "Align backslashes at end of line in region."
  "\\(\\s-*\\)\\\\$")

;;;###autoload (autoload 'nvp-align-hash "nvp-edit")
(nvp-align-fn nvp-align-hash nil "\\(\\s-*\\)#")

;;;###autoload (autoload 'nvp-align-= "nvp-edit")
(nvp-align-fn nvp-align-= nil "\\(\\s-*\\)=")

;; -------------------------------------------------------------------
;;; Insert 

;; Adds commas after numbers in list, like matlab -> R.
;;;###autoload
(defun nvp-list-insert-commas (str &optional from to)
  (interactive
   (if (region-active-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (bounds-of-thing-at-point 'paragraph)))
       (list nil (car bds) (cdr bds)))))
  (let (workonstringp inputstr outputstr)
    (setq workonstringp (if str t nil))
    (setq inputstr (if workonstringp str
                     (buffer-substring-no-properties from to)))
    (setq outputstr
	  (replace-regexp-in-string "\\([0-9]\\)\\s " "\\1," inputstr))
    (if workonstringp
        outputstr
      (save-excursion
        (delete-region from to)
        (goto-char from)
        (insert outputstr)))))

;; -------------------------------------------------------------------
;;; Wrap text

;;;###autoload
(defun nvp-wrap-parens (&optional _arg)
  (interactive "P")
  (sp-wrap-with-pair "("))

;;;###autoload
(defun nvp-wrap-quotes (&optional _arg)
  (interactive "P")
  (if (not (assoc "\"" sp-pair-list))
      (let ((sp-pair-list '(("\"". "\""))))
        (sp-wrap-with-pair "\""))
    (sp-wrap-with-pair "\"")))

;;;###autoload
(defun nvp-wrap-with-squiggles (&optional _arg)
  (interactive "P")
  ;; don't want it in the normal pair list
  (let ((st (make-syntax-table))
        (sp-pair-list '(("~" . "~"))))
    ;; wrap files with suffixes
    (modify-syntax-entry ?. "w" st)
    (with-syntax-table st
      (sp-wrap-with-pair "~"))))

;;;###autoload
(defun nvp-wrap-with-defaults (char &optional _arg)
  "Wrap next sexp with CHAR, where CHAR is the last key pressed.
Uses default vaules for `sp-pair-list'."
  (interactive (list (nvp-last-input-char) current-prefix-arg))
  (sp-wrap-with-pair char))

;;;###autoload
(defun nvp-wrap-with (char &optional _arg)
  "Wrap next sexp with CHAR (last key pressed), overriding default `sp-pair-list'."
  (interactive (list (nvp-last-input-char) current-prefix-arg))
  (let ((sp-pair-list `((,char . ,char))))
    (sp-wrap-with-pair char)))

;; wrap items in list b/w "("..")", defaulting to wrapping with quotes
;;;###autoload (autoload 'nvp-list-wrap-quotes "nvp-edit")
(nvp-wrap-list-items quotes :wrap ("\"" . "\""))

;;;###autoload (autoload 'nvp-list-wrap-parens "nvp-edit")
(nvp-wrap-list-items parens :wrap ("(" . ")"))

;;;###autoload (autoload 'nvp-list-wrap-brackets "nvp-edit")
(nvp-wrap-list-items brackets :wrap ("[" . "]"))

;;;###autoload (autoload 'nvp-list-wrap-squiggles "nvp-edit")
(nvp-wrap-list-items squiggles :wrap ("{" . "}"))

;; -------------------------------------------------------------------
;;; Charset 

;; https://www.emacswiki.org/emacs/UnicodeEncoding
;; Read a unicode code point and insert said character.  Input uses
;; `read-quoted-char-radix' (set to 16 in build/nvp-encoding).
;;;###autoload
(defun nvp-unicode-insert (char)
  (interactive (list (read-quoted-char "Char: ")))
  (insert-char char))

;; -------------------------------------------------------------------
;;; Assorted

;; Print counts of strings in region, with prefix dump at point
;;;###autoload
(defun nvp-stats-uniq (beg end &optional count-lines)
  "Print counts (case-insensitive) of unique words in region BEG to END.
With prefix COUNT-LINES count unique lines."
  (interactive "r\nP")
  (require 'nvp-hash)
  (let ((ht (make-hash-table :test 'case-fold))
        (lines (split-string
                (buffer-substring-no-properties beg end) "\n" 'omit-nulls " "))
        lst)
    (if count-lines
        (dolist (line lines)
          (puthash line (1+ (gethash line ht 0)) ht))
      ;; strip punctuation for words
      (cl-loop for line in lines
         as words = (split-string line "[[:punct:] \t]" 'omit " ")
         when words
         do (cl-loop for word in words
               do (puthash word (1+ (gethash word ht 0)) ht))))
    (maphash (lambda (key val) (push (cons val key) lst)) ht)
    (setq lst (cl-sort lst #'> :key #'car))
    (nvp-with-results-buffer nil
      (pcase-dolist (`(,k . ,v) lst)
        (princ (format "%d: %s\n" k v))))))

(provide 'nvp-edit)
;;; nvp-edit.el ends here
