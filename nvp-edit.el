;;; nvp-edit.el --- Editing -*- lexical-binding: t; -*-
;;; Commentary:
;; Some edit dwim commands: indent, sort, wrap, duplicate lines/regions,
;; un/fill, toggle case, hide/remove lines
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'smartparens))
(require 'transient)
(nvp:decls :p (sp paredit sort) :v (sort-fold-case) :f (paredit-kill))

(autoload 'sp-wrap-with-pair "smartparens")
(autoload 'nvp-read-thing-at-point "nvp-read")
(autoload 'nvp-comment-string "nvp-yas")


;;;###autoload
(defun nvp-indent-dwim (&optional beg end)
  "Indent `thing-at-point' or region between BEG and END.
Defaults to `defun' at point."
  (interactive
   (nvp:with-region beg end
     (nvp:read-char-case "Indent region: " 'verbose
       (?i "[i]buffer" 'buffer)
       (?d "[d]efun" 'defun)
       (?p "[p]aragraph" 'paragraph))
     :pulse t (list beg end)))
  (indent-region beg end))

(defvar nvp-copy-dwim-things '(symbol sexp list defun paragraph)
  "Default list of things to copy at point.")

;;;###autoload
(defun nvp-copy-dwim (&optional beg end prompt things)
  "Copy region from BEG to END or first thing found to the `kill-ring'.
Things are tried in order from THINGS or `nvp-copy-dwim-things'.
With prefix, or when nothing found, PROMPT for thing.
If prefix < 4, start from abs(prefix)'th index in dwim things."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let ((arg (and current-prefix-arg
                     (prefix-numeric-value current-prefix-arg))))
       (list nil nil (and arg (>= arg 4))
             (if (and arg (< arg 4))
                 (nthcdr (abs arg) nvp-copy-dwim-things)
               nvp-copy-dwim-things)))))
  (if (and beg end)
      (copy-region-as-kill beg end)
    (let ((bnds (--some (bounds-of-thing-at-point it)
                        (if prompt
                            (cons (nvp-read-thing-at-point) things)
                          (or things nvp-copy-dwim-things)))))
      (when bnds
        (nvp-indicate-pulse-region-or-line
         (car bnds) (cdr bnds)))
      (if (or bnds
              (null prompt))
          (funcall 'nvp-copy-dwim (car bnds) (cdr bnds) (not bnds) things)
        (user-error "Nothing found for '%S'" things)))))


(defvar duplicate-line-final-position)

;;;###autoload
(defun nvp-duplicate-dwim (&optional arg)
  "Duplicate last (including this) nonempty line ARG times.
If a region or rectangle is active, dupe that instead.
If ARG is <= 0, duplicate ARG'th previous line.
ARG of 0 is treated the same as -1, duplicating the previous line."
  (interactive "p")
  (cl-flet ((goto-nonempty-line (&optional n)
              (let ((col (current-column)))
                (and n (forward-line n))
                (while (and (not (bobp))
                            (nvp:line-empty-p))
                  (forward-line -1))
                (move-to-column col))))
    (cond ((<= (prefix-numeric-value arg) 0)
           (let ((insert-pos (line-beginning-position))
                 (start (progn (goto-nonempty-line (if (zerop arg) -1 arg))
                               (line-beginning-position 2)))
                 (duplicate-line-final-position -1))
             (duplicate-dwim nil)
             (let* ((col (current-column))
                    (end (line-beginning-position 2))
                    (text (buffer-substring start end)))
               (delete-region start end)
               (goto-char insert-pos)
               (insert text)
               (forward-line -1)
               (move-to-column col))))
          ((or (use-region-p)
               (bound-and-true-p rectangle-mark-mode)
               (not (nvp:line-empty-p)))
           (duplicate-dwim arg))
          (t (goto-nonempty-line)
             (duplicate-dwim arg)))))


;;; Sorting

(eval-when-compile
  (defmacro nvp:sort-region (start end &rest body)
    "Sort region between START and END by BODY, using defaults and indent
region afterward."
    (declare (indent defun) (debug (sexp sexp &rest form)))
    (nvp:with-syms (pos)
      `(let ((,pos (point)))
         (save-excursion
           (save-restriction
             (let ((sort-fold-case t))
               (save-match-data ,@body))))
         (indent-region ,start ,end)
         (goto-char ,pos)))))

;;;###autoload
(defun nvp-sort-lines-first-word (start end &optional reverse)
  "Sort lines b/w START and END by first alphanumeric characters.
With prefix sort in REVERSE."
  (interactive `(,@(nvp:tap-or-region 'bdwim 'buffer :pulse t) ,current-prefix-arg))
  (nvp:sort-region start end
    (sort-regexp-fields reverse "^.*$" "\\([[:alnum:]]+\\)" start end)))

;;;###autoload
(defun nvp-sort-lines-first-symbol (beg end &optional reverse no-fold)
  "Sort lines by first symbol.  Symbols are first compared by length and
then using `compare-buffer-substrings'."
  (interactive `(,@(nvp:tap-or-region 'bdwim 'buffer :pulse t) ,current-prefix-arg))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((sort-fold-case (not no-fold))
            (inhibit-field-text-motion t))
        (sort-subr reverse 'forward-line 'end-of-line nil
                   (lambda () (skip-syntax-forward "^ "))
                   (lambda (a b)
                     (let ((l1 (- (cdr a) (car a)))
                           (l2 (- (cdr b) (car b))))
                       (if (= l1 l2)
                           (let ((v (compare-strings
                                     (buffer-substring (car a) (cdr a)) nil nil
                                     (buffer-substring (car b) (cdr b)) nil nil
                                     sort-fold-case)))
                             (or (eq t v) (< v 0)))
                         (<= l1 l2)))))))))

;;;###autoload
(defun nvp-sort-list (&optional start end reverse)
  "Sort list region in list at point or b/w START and END by words/symbols.
With prefix sort in REVERSE."
  (interactive (nvp:with-region start end 'list :pulse t :widen t
                 (list start end current-prefix-arg)))
  (nvp:sort-region start end
    (sort-regexp-fields
     reverse (concat "\\(?:"
                     "\\s\"\\S\"*\\s\"" ; quoted
                     "\\|\\sw+\\|\\s_+" ; word/symbol
                     "\\)")
     "\\(\\sw\\|\\s_\\)+" start end)))

(defun nvp-sort--alist-regexp (start end &optional reverse)
  (nvp:sort-region start end
    (sort-regexp-fields
     ;; Note(09/07/24): keep trailing⬎ comments
     reverse "\\s-*([^\)]*)\\(?:[^\(]*$\\)?" "[^\( \t\n]+"
     (+ start (if (eq ?\' (char-after start)) 2 1)) ; skip over outer '('
     (1- end))))

;; Note: uses 'cons/'alist at point defined in nvp-elisp
;;;###autoload
(defun nvp-sort-alist (&optional start end use-regexp &rest args)
  "Sort alist (or improper list of conses) at point or from START to END.
With \\[universal-argument], reverse ordering.

With prefix \\='-, or USE-REGEXP, or the thing at point looks like bindings,
sort with regexps instead of reading the sexp. This also preserves comments.
NOTE: when not sorting by USE-REGEXP, comments are erased in the list!!

ARGS are key-value pairs passed to `sort'."
  (interactive
   (let ((regexp-p (eq '- current-prefix-arg)))
     `(,@(or (and (use-region-p)
                  (list (region-beginning) (region-end)))
             (save-restriction
               (widen)
               (let ((bnds
                      (or (bounds-of-thing-at-point 'alist)
                          (save-excursion
                            (and-let* ((bnds (bounds-of-thing-at-point 'cons)))
                              (goto-char (1- (car bnds))))
                            (and-let* ((bnds (bounds-of-thing-at-point 'list)))
                              (goto-char (1+ (car bnds)))
                              (while (and (< (point) (cdr bnds))
                                          (not (eq ?\( (char-after))))
                                (forward-sexp)
                                (skip-chars-forward " \t\n"))
                              (when (and (eq ?\( (char-after))
                                         (save-excursion
                                           (forward-char)
                                           (thing-at-point 'cons)))
                                (setq regexp-p t)
                                (cons (1- (point)) (cdr bnds))))))))
                 (if (null bnds)
                     (list nil nil)
                   (nvp-indicate-pulse-region-or-line (car bnds) (cdr bnds))
                   (list (car bnds) (cdr bnds))))))
       ,regexp-p
       :key car
       :reverse ,current-prefix-arg)))
  (unless (and start end)
    (user-error "No alist at point."))
  (if use-regexp
      (nvp-sort--alist-regexp start end (plist-get args :reverse))
    (replace-region-contents
     start end
     (lambda ()
       (let ((alist (apply #'sort (read (current-buffer)) args)))
         (with-temp-buffer
           (let ((cl-print-readably t)
                 (print-quoted t)
                 (print-circle nil)
                 (print-escape-newlines nil)
                 (pp-max-width fill-column)
                 (pp-default-function 'pp-29)
                 (standard-output (current-buffer))
                 cur)
             (insert "(")
             (while (setq cur (car alist))
               (setq alist (cdr alist))
               (cl-prin1 cur)
               (and alist (terpri)))
             (insert ")")
             (pp-buffer))
           (buffer-string))))
     0 0)
    (setq start (point))
    (save-excursion
      (goto-char (scan-lists (point) 1 0))
      (setq end (point))
      (unless (zerop (skip-chars-forward " \t\n"))
        (delete-region end (point)))
      (or (eq ?\) (char-after))
          (insert "\n"))
      (indent-region start (line-end-position)))))

;;;###autoload
(defun nvp-sort-words (start end &optional reverse)
  "Sort words (non-whitespace considered a word) in region."
  (interactive (nvp:with-region start end 'list :pulse t :widen t
                 (list start end current-prefix-arg)))
  (nvp:sort-region start end
    (sort-regexp-fields reverse "[^ \t\n]+" "\\&" start end)))

;;;###autoload
(defun nvp-sort-symbols (start end &optional reverse)
  "Sort symbols in region."
  (interactive (nvp:with-region start end 'list :pulse t :widen t
                 (list start end current-prefix-arg)))
  (nvp:sort-region start end
    (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" start end)))

;;;###autoload(autoload 'nvp-sort-menu "nvp-edit" nil t)
(transient-define-prefix nvp-sort-menu ()
  [["Lines"
    ("l" "Alphabetically" sort-lines)
    ("f" "By field" sort-fields)
    ("r" "By regex field" sort-regexp-fields)
    ("n" "By numeric field" sort-numeric-fields)
    ("F" "By first word" nvp-sort-lines-first-word)
    ("s" "By first symbol" nvp-sort-lines-first-symbol)
    ("c" "Columns" sort-columns)
    ("R" "Reverse lines" reverse-region)]
   ["Lists"
    ("a" "Alists/Bindings" nvp-sort-alist)
    ("A" "Abbrevs" nvp-abbrev-sort-table)
    ("L" "List" nvp-sort-list)]
   ["Region"
    ("S" "Symbols" nvp-sort-symbols)
    ("w" "Words" nvp-sort-words)]])


;;; Wrapping

;;;###autoload
(defun nvp-wrap-with-squiggles (&optional _arg)
  (interactive "P")
  ;; Don't want it in the normal pair list
  (let ((st (make-syntax-table))
        (sp-pair-list '(("~" . "~"))))
    ;; Wrap files with suffixes
    (modify-syntax-entry ?. "w" st)
    (with-syntax-table st
      (sp-wrap-with-pair "~"))))

;;;###autoload
(defun nvp-wrap-with-last-char (&optional char prompt)
  "Wrap next sexp with CHAR (last key pressed in calling command).
Override default `sp-pair-list' if CHAR isn't a leading member.
With prefix, read CHAR to wrap with."
  (interactive (list (unless current-prefix-arg
                       (nvp:input 'lcs))
                     current-prefix-arg))
  (when (or prompt (null char))
    (setq char (char-to-string (read-char "Wrap with: ")))
    (nvp:prefix-shift -1))
  (let* ((pair (cons char char))
         ;; (sp-wrap-from-point t)
         ;; (sp-wrap-entire-symbol t)
         (sp-pair-list
          (if prompt (cons pair sp-pair-list)
            (append sp-pair-list (list pair)))))
    (with-demoted-errors "Error in nvp-wrap-with-last-char: %S"
      (sp-wrap-with-pair char))))


;;; Edit Lines

(declare-function consult-focus-lines "consult")
(defvar consult--focus-lines-overlays)

(define-minor-mode nvp-consult-focus-minor-mode
  "Minor mode active when buffer is focused."
  :lighter (:propertize " Focus" face font-lock-warning-face)
  :interactive nil
  (cond (nvp-consult-focus-minor-mode
         (or consult--focus-lines-overlays
             (nvp-consult-focus-minor-mode -1)))
        (t (and consult--focus-lines-overlays
                (nvp-consult-focus-minor-mode 1))))
  (remove-hook 'post-command-hook #'nvp-consult-focus-minor-mode))

;;;###autoload
(defun nvp-consult-focus-lines (&optional keep-focus)
  "Toggle hiding/showing matching lines.
With prefix KEEP-FOCUS, keep hiding regardless."
  (interactive (list (or current-prefix-arg
                         (nvp:transient-arg "--keep-focus"))))
  (if (and consult--focus-lines-overlays
           (not keep-focus))
      (progn (funcall #'consult-focus-lines nil t)
             (nvp-consult-focus-minor-mode -1))
    (add-hook 'post-command-hook #'nvp-consult-focus-minor-mode)
    (call-interactively #'consult-focus-lines)))

;;;###autoload(autoload 'nvp-edit-lines-menu "nvp-edit" nil t)
(transient-define-prefix nvp-edit-lines-menu ()
  "Edit lines"
  :refresh-suffixes t
  [["Hide"
    ("h" "Toggle('!')" nvp-consult-focus-lines :transient t)
    ("-k" "Keep hidden" ("--keep-focus" "--keep-focus")
     :if-non-nil consult--focus-lines-overlays)
    ""
    "Kill/Copy"
    ("k" "Kill" kill-matching-lines)
    ("c" "Copy" copy-matching-lines)]
   ["Keep"
    ("l" "Matches('!')" consult-keep-lines)
    ("L" "keep-lines" keep-lines)
    ""
    "Delete"
    ("d" "Matches" flush-lines)
    ("w" "Trailing ws" delete-trailing-whitespace)]
   ["Other"
    ("#" "Count (>pos)" how-many :transient t)]])


;;; Toggle Case

(defvar-keymap nvp-repeat-case-map
  :repeat t
  ;; When repeating, "u"/"U" will switch between capitalizing or
  ;; downcase/upcasing
  "u" #'nvp-toggle-case-dwim
  "U" #'nvp-toggle-case-dwim)

;;;###autoload
(defun nvp-toggle-case-dwim (arg &optional capitalize beg end last-action
                                 _last-char)
  "Change case of ARG words at point.
With prefix 0, or when last input char was lowercase, CAPITILIZE them.
When region is active or BEG and END are non-nil, operate on region.
LAST-ACTION is used during repeats."
  (interactive
   (let ((c (nvp:input 'lcs)))
     (nvp:repeat-args
       :repeat-p (lambda () (eq c (nth 5 (get this-command 'repeat-args))))
       (let ((arg (abs (prefix-numeric-value current-prefix-arg))))
         `(,(max 1 arg)
           ,(or (not (nvp:upcase-p c)))
           ,@(if (use-region-p)
                 (list (region-beginning) (region-end))
               (list nil nil))
           nil ,c)))))
  (let* ((region-p (or (and beg end)
                       (use-region-p)))
         (action
          (cond ((and last-action region-p)
                 (if (eq last-action 'downcase) 'upcase 'downcase))
                (last-action last-action)
                (capitalize 'capitalize)
                (t (save-mark-and-excursion
                     (when region-p
                       (goto-char beg))
                     (skip-syntax-forward "<\" _'.\(")
                     (let (case-fold-search)
                       (if (looking-at-p "\\s-*[A-Z0-9_:./(){}-]+\\b")
                           'downcase
                         'upcase))))))
         (fn (intern (format "%S-%s" action (if region-p "region" "word"))))
         (this-args (get this-command 'repeat-args)))
    (setf (nthcdr (1- (length this-args)) this-args)
          (list action))
    (when (and beg end
               (not (region-active-p)))
      (nvp-indicate-pulse-region-or-line beg end))
    (apply fn (if region-p
                  (list beg end)
                (list arg)))))


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
  (interactive (let ((arg (prefix-numeric-value current-prefix-arg)))
                 (list (and (eq 4 arg)
                            (read-number "Fill column: " fill-column))
                       (eq 16 arg))))
  (let ((fill-column (or column (nvp:toggled-if fill-column
                                  most-positive-fixnum)))
        ;; TODO(08/26/24): treat notes, fixmes, etc. as paragraph breaks
        (paragraph-start (if (nvp:ppss 'cmt)
                             (nvp-comment-string "" 2)
                           paragraph-start)))
    (if (region-active-p)
        (call-interactively #'fill-region)
      (let ((fill-fn (or nvp-fill-paragraph-function
                         fill-paragraph-function
                         'prog-fill-reindent-defun)))
        (deactivate-mark t)
        (funcall (if (commandp fill-fn)
                     'funcall-interactively
                   'funcall)
                 fill-fn (and justify 'fill-paragraph))))))


(provide 'nvp-edit)
;; local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-edit.el ends here
