;;; nvp-edit-aux.el --- rarely used -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Lesser used stuff:
;; - list wrapping, insert commas, quote elements
;; - fill/unfill
;; - rarely used paredit commands
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'transient)
(nvp:decls :f (paredit-delete-indentation))

(autoload 'nvp-comment-string "nvp-yas")


(nvp:bindings nvp-list-keymap nil
  :prefix "List"
  ("<f2>" . nvp-list-menu)
  (","    . nvp-list-insert-commas))

(dolist (c '("(" "[" "{" "\"" "'" "~" "*" "`" "=" "_" "/"))
  (define-key nvp-list-keymap c #'nvp-list-wrap))

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

;; -------------------------------------------------------------------
;;; Lists

(defvar nvp-list-pairs '(("{" . "}")
                         ("(" . ")")
                         ("\"" . "\"")
                         ("[" . "]")
                         ("'" . "'")))

(defvar nvp-list-element-re "[^ )(\t\n,']+" "Regexp matching a list element.")

(defvar nvp-list-sep-re "[, \t\n]" "Regexp matching list separator.")

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
  (let* ((closer closer)
         (opener (if prompt
                     (let ((pair (read--expression "Wrap items with (a . b): ")))
                       (cl-assert (or (consp pair) (stringp pair)))
                       (setq closer (if (consp pair) (cdr pair)
                                      (or (assoc-default pair nvp-list-pairs) pair)))
                       (if (consp pair) (car pair) pair))
                   (let ((opener (or opener (nvp:input 'lcs))))
                     (setq closer (or (assoc-default opener nvp-list-pairs) opener))
                     opener)))
         (elem-re (concat "\\s-*\\(" (or nvp-list-element-re "[^ \t\n,]+")
                          "\\)" (or sep nvp-list-sep-re "[, \t\n]") "*"))
         (str (buffer-substring-no-properties start end)))
    (delete-region start end)
    (insert (with-temp-buffer
              (insert str)
              (goto-char (point-min))
              (while (re-search-forward elem-re nil t)
                (replace-match
                 (concat opener (match-string-no-properties 1) closer)
                 t nil nil 1))
              (buffer-substring-no-properties (point-min) (point-max))))))

;; Adds commas after numbers in list, like matlab -> R.
(defun nvp-list-insert-commas (str &optional beg end)
  (interactive (nvp:with-region beg end 'list :pulse t (list nil beg end)))
  (let ((res (replace-regexp-in-string
              "\\([0-9]\\)\\s-+" "\\1, "
              (or str (buffer-substring-no-properties beg end)))))
    (if str res
      (save-excursion
        (delete-region beg end)
        (goto-char beg)
        (insert res)))))

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
  "Unfill the region, joining text paragraphs into a single logical line.
This is useful, e.g, for use with `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

(defvar-keymap nvp-repeat-fill-paragraph-map
  :repeat t
  "q" #'nvp-fill-paragraph-toggle)

;;;###autoload
(defun nvp-fill-paragraph-toggle (&optional column justify)
  "Toggle paragraph filling.
With prefix, prompt for `fill-column'. With two prefix, justify as well."
  (interactive
   (list (and (eq 4 (prefix-numeric-value current-prefix-arg))
              (read-number "Fill column: " fill-column))
         (eq 16 (prefix-numeric-value current-prefix-arg))))
  (let ((fill-column
         (or column
             (nvp:toggled-if fill-column most-positive-fixnum)))
        (paragraph-start
         ;; TODO(08/26/24): treat notes, fixmes, etc. as paragraph breaks
         (if (nvp:ppss 'cmt)
             (nvp-comment-string "" 2)
           paragraph-start)))
    (if (region-active-p)
        (call-interactively #'fill-region)
      (let ((fill-fn (or nvp-fill-paragraph-function
                         fill-paragraph-function
                         #'prog-fill-reindent-defun)))
        (deactivate-mark t)
        (funcall (if (commandp fill-fn) 'funcall-interactively 'funcall)
                 fill-fn (and justify 'fill-paragraph))))))

(provide 'nvp-edit-aux)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-edit-aux.el ends here
