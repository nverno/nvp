;;; nvp-edit --- 

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
  (require 'cl-lib))

;; convert selected bindings to macro form and align
;;;###autoload
(defun nvp-macroify-bindings (start end)
  (interactive "r")
  (goto-char start)
  (let ((map (save-excursion
               (when (re-search-forward "\\([a-zA-Z0-9-]+\\)-map"
                                        end t)
                 (match-string-no-properties 1)))))
    (when map
      (let (binds)
        (while (re-search-forward
                "\\(\"[^\"]+\"\\))?[\n\t ]*[#']*\\([a-zA-Z0-9-]+\\)"
                end t)
          (push (format "(%s . %s)"
                        (match-string-no-properties 1)
                        (match-string-no-properties 2))
                binds))
        (goto-char start)
        (insert (concat "(nvp-bindings \"" map "\" nil \n  "
                        (mapconcat 'identity (nreverse binds) "\n  ")
                        ")\n"))
        (goto-char start)
        (mark-sexp)
        (align-regexp (region-beginning) (region-end)
                      "\\(\\s-*\\)\\. ")))))

;;--- Sort -----------------------------------------------------------

;; Sort region by first character of each line. With prefix, reverse
;; the sort
;;;###autoload
(defun nvp-sort-lines-first-char (start end &optional reverse)
  (interactive "r\nP")
  (let ((sort-fold-case t))
    (sort-regexp-fields reverse "^.*$" "\\([a-zA-Z]\\)" start end)))

;; Sort list by first character, with prefix do reverse.
;;;###autoload
(defun nvp-sort-list (start end &optional reverse)
  (interactive "r\nP")
  (let ((sort-fold-case t))
    (sort-regexp-fields reverse "[^ \t\n]+" "\\([a-zA-Z]\\)"
                        start end)))

;;;###autoload
(defun nvp-sort-words (start end &optional reverse)
  (interactive "r\nP")
  (let ((sort-fold-case t))
    (sort-regexp-fields reverse "[^ \t\n]+" "\\&" start end)))

;;;###autoload
(defun nvp-sort-symbols (start end &optional reverse)
  (interactive "r\nP")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" start end))

;;--- Fill -----------------------------------------------------------

;; Convert multiple line paragraph into one line.
;;;###autoload
(defun nvp-unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; Remove newlines in list, leaving single spaces.
;;;###autoload
(defun nvp-unfill-list (begin end)
  (interactive "r")
  (save-excursion
    (narrow-to-region begin end)
    (goto-char begin)
    (while (search-forward-regexp "[ \n\t\r]+" end t)
      (replace-match " " nil t))
    (widen)
    (let ((fill-column 75))
      (fill-paragraph))))

;;--- Align ----------------------------------------------------------

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

;; Align comments within marked regions.
;;;###autoload
(defun nvp-align-comments (beg end)
  (interactive "*r")
  (let (indent-tabs-mode align-to-tab-stop)
    (if (not (eq major-mode 'c-mode))
        (align-regexp beg end (concat "\\(\\s-*\\)"
                                      (regexp-quote comment-start)))
      (align-regexp beg end (concat "\\(\\s-*\\)\\(?://\\|/\\*\\)")))))

;;;###autoload (autoload 'nvp-align-backslash "nvp-align")
(nvp-align-fn nvp-align-backslash
  "Align backslashes at end of line in region."
  "\\(\\s-*\\)\\\\$")

;;;###autoload (autoload 'nvp-align-hash "nvp-align")
(nvp-align-fn nvp-align-hash
  "Align '#' in region."
  "\\(\\s-*\\)#")

;;--- Insert ---------------------------------------------------------

;; Adds commas after numbers in list, like matlab -> R.
;;;###autoload
(defun nvp-insert-commas (str &optional from to)
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

;; insert a lorem ipsum spiel
;;;###autoload
(defun nvp-lorem ()
  (interactive)
  (insert
   "lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
   "eiusmod tempor incididunt ut labore et dolore magna aliqua. ut enim"
   "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
   "aliquip ex ea commodo consequat. duis aute irure dolor in "
   "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
   "pariatur. excepteur sint occaecat cupidatat non proident, sunt in "
   "culpa qui officia deserunt mollit anim id est laborum."))

;;--- Wrap -----------------------------------------------------------

;;;###autoload
(defun nvp-wrap-parens (&optional arg)
  (interactive "P")
  (sp-wrap-with-pair "("))

;;;###autoload
(defun nvp-wrap-quotes (&optional arg)
  (interactive "P")
  (sp-wrap-with-pair "\""))

;; wrap items in list b/w "("..")", defaulting to wrapping with quotes
;;;###autoload (autoload 'nvp-list-wrap-parens "nvp-list")
(nvp-wrap-list-items parens :wrap ("\"" . "\""))

;;--- Charset --------------------------------------------------------

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
