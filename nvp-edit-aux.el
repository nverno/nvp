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
