;;; nvp-edit ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 24 November 2016

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

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

(defmacro nvp-sort-with-defaults (start end &rest body)
  "Sort region between START and END by BODY, using defaults and indent region \
afterward."
  (declare (indent defun))
  `(let ((sort-fold-case t))
     ,@body
     (indent-region ,start ,end)))

;;;###autoload
(defun nvp-sort-lines-first-word (start end &optional reverse)
  "Sort lines b/w START and END by first alphanumeric characters.
With prefix sort in REVERSE."
  (interactive "r\nP")
  (nvp-sort-with-defaults start end
    (sort-regexp-fields reverse "^.*$" "\\([[:alnum:]]+\\)" start end)))

;;;###autoload
(defun nvp-sort-list (start end &optional reverse)
  "Sort list b/w START and END by words/symbols.
With prefix sort in REVERSE."
  (interactive "r\nP")
  (nvp-sort-with-defaults start end
    (sort-regexp-fields reverse (nvp-concat "\\(?:"
                                            "\\s\"\\S\"*\\s\"" ;quoted
                                            "\\|\\sw+\\|\\s_+" ;word/symbol
                                            "\\)")
                        "\\(\\sw\\|\\s_\\)+" start end)))

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

;;;###autoload
(defun nvp-fill-paragraph-toggle ()
  (interactive)
  (let (deactivate-mark
        (fill-column
         (if (not (eq last-command this-command))
             fill-column
           (setq this-command nil)
           most-positive-fixnum)))
    (call-interactively 'fill-paragraph)))

;; -------------------------------------------------------------------
;;; Align 

;;;###autoload
(defadvice align-regexp (around align-regexp-with-spaces)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))
;;;###autoload
(ad-activate 'align-regexp)

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

;; Print the ascii table.
;;;###autoload
(defun nvp-ascii-table ()
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (cl-loop
     for i from 0 to 15
     do (cl-loop
           for j from 0 to 15
           do
             (when (= j 0) (insert (format "%4d |" i)))
             (insert (format " %c " (+ i (* j 16))))
             (when (= j 15) (insert "\n")))))

(provide 'nvp-edit)
;;; nvp-edit.el ends here
