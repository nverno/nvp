;;; nvp-list.el --- Lists -*- lexical-binding: t; -*-
;;; Commentary:
;; - Wrap regions in lists
;; - quote/separate list elements
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'transient)
(nvp:decls :f (paredit-delete-indentation))


(nvp:bindings nvp-list-keymap nil
  :prefix "List"
  ("<f2>" . nvp-list-menu)
  (","    . nvp-list-insert-commas))

(dolist (c '("(" "[" "{" "\"" "'" "~" "*" "`" "=" "_" "/"))
  (define-key nvp-list-keymap c #'nvp-list-wrap))

(defvar nvp-list-pairs '(("{" . "}")
                         ("(" . ")")
                         ("\"" . "\"")
                         ("[" . "]")
                         ("'" . "'")))

(defvar nvp-list-element-re "[^ )(\t\n,']+"
  "Regexp matching a list element.")

(defvar nvp-list-sep-re "[, \t\n]"
  "Regexp matching list separator.")


;;;###autoload
(defun nvp-list-wrap (start end &optional opener closer sep prompt)
  "Wrap list elements between START and END with OPENER and CLOSER.
OPENER defaults to last basic input character.
CLOSER defaults to matching element in `nvp-list-pairs', or OPENER.
SEP is a regexp matching list element separators.
If PROMPT is non-nil, prompt for OPENER/CLOSER."
  (interactive (let ((bnds (nvp:tap 'bdwim 'list nil nil :pulse t)))
                 (list (car bnds) (cdr bnds) nil nil nil current-prefix-arg)))
  (unless (and start end (< start end))
    (user-error "No region"))

  (cl-labels ((as-string (exp)
                (cond ((stringp exp) exp)
                      ((symbolp exp) (symbol-name exp))
                      ((characterp exp) (char-to-string exp))
                      ((consp exp) (cons (as-string (car exp))
                                         (as-string (cdr exp))))
                      (t (user-error "dont know about \"%S\"" exp)))))
    (if prompt
        (let ((pre (as-string (read--expression "Wrap items with (a . b): "))))
          (if (consp pre) (setq opener (car pre)
                                closer (cdr pre))
            (setq opener pre
                  closer (or (assoc-default pre nvp-list-pairs) pre))))
      (or opener (setq opener (nvp:input 'lcs)))
      (or closer (setq closer (or (assoc-default opener nvp-list-pairs)
                                  opener))))
    (let ((elem-re (concat "\\s-*\\("
                           (or nvp-list-element-re "[^ \t\n,]+") "\\)"
                           (or sep nvp-list-sep-re "[, \t\n]") "*"))
          (str (buffer-substring-no-properties start end)))
      (delete-region start end)
      (insert (with-temp-buffer
                (insert str)
                (goto-char (point-min))
                (while (re-search-forward elem-re nil t)
                  (replace-match
                   (concat opener (match-string-no-properties 1) closer)
                   t nil nil 1))
                (buffer-substring-no-properties (point-min) (point-max)))))))

(defun nvp-list-insert-commas (str &optional beg end)
  "Insert commas after numbers in list (eg. matlab -> R)."
  (interactive (nvp:with-region beg end 'list :pulse t (list nil beg end)))
  (let ((res (replace-regexp-in-string
              "\\([0-9]\\)\\s-+" "\\1, "
              (or str (buffer-substring-no-properties beg end)))))
    (if str res
      (save-excursion
        (delete-region beg end)
        (goto-char beg)
        (insert res)))))


;;; List Menu

(transient-define-infix nvp-list-menu--element-re ()
  :class 'transient-lisp-variable
  :variable 'nvp-list-element-re)

(transient-define-infix nvp-list-menu--sep-re ()
  :class 'transient-lisp-variable
  :variable 'nvp-list-sep-re)

(transient-define-prefix nvp-list-menu ()
  [["Wrap"
    ("(" "()" nvp-list-wrap)
    ("{" "{}" nvp-list-wrap)
    ("[" "[]" nvp-list-wrap)
    ("\"" "\"\"" nvp-list-wrap)
    ("'" "''" nvp-list-wrap)]
   ["Wrap"
    ("~" "~" nvp-list-wrap)
    ("*" "*" nvp-list-wrap)
    ("=" "=" nvp-list-wrap)
    ("_" "_" nvp-list-wrap)
    ("/" "/" nvp-list-wrap)]
   ["Insert"
    ("," "x[, xs]*" nvp-list-insert-commas)]]
  ["Configure"
   (":e" "Element regexp" nvp-list-menu--element-re)
   (":s" "Separator regexp" nvp-list-menu--sep-re)])


(provide 'nvp-list)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-list.el ends here
