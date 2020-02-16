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
(nvp-decls :f (paredit-delete-indentation))

(nvp-bindings nvp-list-keymap :now
  :create t
  ("c"  . nvp-list-insert-commas)
  ("("  . nvp-list-wrap-parens)
  ("["  . nvp-list-wrap-brackets)
  ("{"  . nvp-list-wrap-squiggles)
  ("\"" . nvp-list-wrap-quotes))

;; -------------------------------------------------------------------
;;; Lists

;; Adds commas after numbers in list, like matlab -> R.
(defun nvp-list-insert-commas (str &optional beg end)
  (interactive (nvp-with-region beg end 'list :pulse t (list nil beg end)))
  (let ((res (replace-regexp-in-string
              "\\([0-9]\\)\\s-+" "\\1, "
              (or str (buffer-substring-no-properties beg end)))))
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


;; -------------------------------------------------------------------
;;; Paredit -- little used commands

;;;###autoload
(defun nvp-paredit-remove-newlines ()
  "Removes extra whitespace and newlines from current point to the next paren."
  (interactive)
  (let ((up-to (point)))
    (backward-char)
    (while (> (point) up-to)
      (nvp-paredit-delete-indentation))))

;; https://www.emacswiki.org/emacs/ParEdit
(defun nvp-paredit-delete-indentation (&optional arg)
  "Handle joining lines that end in a comment."
  (interactive "P")
  (let (comt)
    (save-excursion
      (move-beginning-of-line (if arg 1 0))
      (when (skip-syntax-forward "^<" (nvp-point 'eol))
        (setq comt (delete-and-extract-region (point) (nvp-point 'eol))))
      (delete-indentation arg)
      (when comt
        (save-excursion
          (move-end-of-line 1)
          (insert " ")
          (insert comt))))))


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
(defun nvp-fill-paragraph-toggle (&optional column)
  "Toggle paragraph filling. With prefix, prompt for `fill-column'."
  (interactive (list (and current-prefix-arg (read-number "Fill column: "))))
  (let ((fill-column (or column (nvp-toggled-if fill-column most-positive-fixnum))))
    (deactivate-mark t)
    (call-interactively 'fill-paragraph))
  (nvp-repeat-command))

(provide 'nvp-edit-aux)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-edit-aux.el ends here
