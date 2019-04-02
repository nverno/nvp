;;; nvp-edit.el --- editing autoloads -*- lexical-binding: t; -*-

;; Last modified: <2019-04-01.15>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 24 November 2016

;;; Commentary:

;; FIXME:
;; - fill functions are pretty useless
;; - fill toggle doesn't work much

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

(eval-when-compile
  (defmacro nvp-sort-with-defaults (start end &rest body)
    "Sort region between START and END by BODY, using defaults and indent \
region afterward."
    (declare (indent defun) (debug (sexp sexp &rest form)))
    `(let ((sort-fold-case t))
       (save-mark-and-excursion
         (save-match-data
           (unwind-protect
               ,@body
             (indent-region ,start ,end)))))))

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
  (interactive (nvp-with-region start end 'list :pulse t :widen t
                 (list start end current-prefix-arg)))
  (nvp-sort-with-defaults start end
    (sort-regexp-fields reverse (nvp-concat "\\(?:"
                                            "\\s\"\\S\"*\\s\"" ;quoted
                                            "\\|\\sw+\\|\\s_+" ;word/symbol
                                            "\\)")
                        "\\(\\sw\\|\\s_\\)+" start end)))

;; note uses 'cons/'alist at point defined in nvp-elisp
;;;###autoload
(defun nvp-sort-alist (&optional start end reverse)
  "Sort alist by car of each element in list at point or b/w START and END."
  (interactive (nvp-with-region start end 'alist :pulse t :widen t
                 (list start end current-prefix-arg)))
  (nvp-sort-with-defaults start end
    (sort-regexp-fields
     reverse "\\s-*([^\)]*)\\(?:[^\(]*$\\)?" "\\([[:alnum:]]\\)"
     (if (looking-at-p "'") (+ 2 start) (1+ start)) ;skip over outer '('
     (1- end))))                                   ;stop before final ')'

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

;;;###autoload
(defun nvp-unfill-list (begin end &optional regexp)
  "Remove newlines in list, leaving single spaces."
  (interactive "r")
  (setq regexp (if current-prefix-arg
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

;;;###autoload
(defun nvp-unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (list t))
  (let ((fill-column (point-max))
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;;;###autoload
(defun nvp-unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single logical line. This
is useful, e.g, for use with `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

;;;###autoload
(defun nvp-fill-paragraph-toggle ()
  (interactive)
  (let (deactivate-mark
        (fill-column (nvp-toggled-if fill-column most-positive-fixnum)))
    (call-interactively 'fill-paragraph)))

;; -------------------------------------------------------------------
;;; Align 

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

;; -------------------------------------------------------------------
;;; Wrap text

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
(defun nvp-wrap-with-last-char (char &optional _arg)
  "Wrap next sexp with CHAR (last key pressed in calling command).
Override default `sp-pair-list' if CHAR isn't a leading member.
Prefix arg is passed to SP, wrapping the next _ARG elements."
  (interactive (list (nvp-input 'lce) current-prefix-arg))
  (let ((sp-pair-list
         (if (not (cl-member char sp-pair-list :test #'string= :key #'car))
             `((,char . ,char))
           sp-pair-list)))
    (with-demoted-errors "Error in nvp-wrap-with-last-char: %S"
      (sp-wrap-with-pair char))))

;; -------------------------------------------------------------------
;;; Lists

;; Adds commas after numbers in list, like matlab -> R.
(defun nvp-list-insert-commas (str &optional beg end)
  (interactive (nvp-with-region beg end 'list :pulse t (list nil beg end)))
  (let ((res (replace-regexp-in-string
              "\\([0-9]\\)\\s-+" "\\1, "
              (or str (nvp-s 'rs beg end)))))
    (if str res
      (save-excursion
        (delete-region beg end)
        (goto-char beg)
        (insert res)))))

(eval-when-compile
  ;; Wrap items in list between DELIM, default wrapping with WRAP
  ;; Create list wrapping functions, wrapping items between DELIMS with
  ;; WRAP by default, prompting for wrapping string with prefix.  IGNORE
  ;; is regexp to ignore in list, ie commas and spaces and MATCH is regex
  ;; to capture items.
  (cl-defmacro nvp-wrap-list-items (name
                                    &key
                                    (delims '("(" . ")"))
                                    (match "[^)(, \n\t\r]+")
                                    (ignore "[, \n\t\r]*")
                                    (wrap '("\"" . "\"")))
    (declare (debug defun)
             (indent defun))
    (let ((fn (intern (concat "nvp-list-wrap-" (symbol-name name))))
          (doc (format
                (concat "Wrap items of list in selected region between "
                        "%s...%s with items with %s..%s by default or "
                        "string ARG, prompting with prefix.")
                (car delims) (cdr delims) (car wrap) (cdr wrap)))
          (delim-re (concat ignore "\\(" match "\\)")))
      `(defun ,fn (start end &optional arg)
         ,doc
         (interactive "r\nP")
         (let* ((wrap (or (and arg
                               (car
                                (read-from-string
                                 (read-from-minibuffer
                                  "Wrap items with(a . b): "))))
                          ',wrap))
                (str (buffer-substring-no-properties start end)))
           (delete-region start end)
           (insert
            (with-temp-buffer
              (insert str)
              (goto-char (point-min))
              (re-search-forward ,(regexp-quote (car delims)) nil t)
              (while (re-search-forward ,delim-re nil t)
                (replace-match (concat (car wrap)
                                       (match-string-no-properties 1)
                                       (cdr wrap))
                               t nil nil 1))
              (buffer-string))))))))

;; FIXME: Combine into single interface, wrapping with next character
;;        Also, become syntax aware
;; wrap items in list b/w "("..")", defaulting to wrapping with quotes
(nvp-wrap-list-items quotes :wrap ("\"" . "\""))
(nvp-wrap-list-items parens :wrap ("(" . ")"))
(nvp-wrap-list-items brackets :wrap ("[" . "]"))
(nvp-wrap-list-items squiggles :wrap ("{" . "}"))

;; -------------------------------------------------------------------
;;; Charset 

;; https://www.emacswiki.org/emacs/UnicodeEncoding
;; Read a unicode code point and insert said character.  Input uses
;; `read-quoted-char-radix' (set to 16 in build/nvp-encoding).
(defun nvp-unicode-insert (char)
  (interactive (list (read-quoted-char "Char: ")))
  (insert-char char))

(provide 'nvp-edit)
;;; nvp-edit.el ends here
