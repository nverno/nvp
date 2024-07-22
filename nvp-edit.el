;;; nvp-edit.el --- Editing -*- lexical-binding: t; -*-
;;; Commentary:
;; Some random editing things: indent, sort, wrap, duplicate lines/regions
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'smartparens))
(require 'transient)
(nvp:decls :p (sp paredit sort) :v (sort-fold-case) :f (paredit-kill))

(autoload 'sp-wrap-with-pair "smartparens")


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

;;;###autoload
(defun nvp-duplicate-dwim (&optional arg)
  "Duplicate last (including this) nonempty line ARG times.
If a region or rectangle is active, dupe that instead."
  (interactive "p")
  (if (or (use-region-p)
          (bound-and-true-p rectangle-mark-mode)
          (not (nvp:line-empty-p)))
      (duplicate-dwim arg)
    (let ((col (current-column)))
      (while (and (not (bobp))
                  (nvp:line-empty-p))
        (forward-line -1))
      (move-to-column col)
      (duplicate-dwim arg))))


;;; Sorting

(eval-when-compile
  (defmacro nvp:sort-region (start end &rest body)
    "Sort region between START and END by BODY, using defaults and indent
region afterward."
    (declare (indent defun) (debug (sexp sexp &rest form)))
    `(save-excursion
       (unwind-protect
           (save-restriction
             (save-match-data
               (let ((sort-fold-case t))
                 ,@body)))
         (indent-region ,start ,end)))))

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
    (sort-regexp-fields reverse (nvp:concat "\\(?:"
                                            "\\s\"\\S\"*\\s\"" ;quoted
                                            "\\|\\sw+\\|\\s_+" ;word/symbol
                                            "\\)")
                        "\\(\\sw\\|\\s_\\)+" start end)))

;; note uses 'cons/'alist at point defined in nvp-elisp
;;;###autoload
(defun nvp-sort-alist (&optional start end reverse)
  "Sort alist by car of each element in list at point or b/w START and END."
  (interactive (nvp:with-region start end 'alist :pulse t :widen t
                 (list start end current-prefix-arg)))
  (nvp:sort-region start end
    (sort-regexp-fields
     reverse "\\s-*([^\)]*)\\(?:[^\(]*$\\)?" "\\([[:alnum:]]\\)"
     (if (looking-at-p "'") (+ 2 start) (1+ start)) ;skip over outer '('
     (1- end))))                                    ;stop before final ')'

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
   ["List"
    ("a" "Alist" nvp-sort-alist)
    ("L" "List" nvp-sort-list)]
   ["Region"
    ("S" "Symbols" nvp-sort-symbols)
    ("w" "Words" nvp-sort-words)]])


;;; Wrapping

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
  (interactive (list (nvp:input 'lcs) current-prefix-arg))
  (let ((sp-pair-list
         (if (not (cl-member char sp-pair-list :test #'string= :key #'car))
             `((,char . ,char))
           sp-pair-list)))
    (with-demoted-errors "Error in nvp-wrap-with-last-char: %S"
      (sp-wrap-with-pair char))))

(provide 'nvp-edit)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-edit.el ends here
