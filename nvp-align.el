;;; nvp-align.el --- alignment -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Alignment:
;; - interactive align commands
;; - show rules applicable for mode
;; - additional alignment rules
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'align)
(nvp-decls :f (nvp-read-mode))

(nvp-bindings nvp-align-keymap nil
  :create t
  ("A"   . align)
  ("a"   . nvp-align)
  ("C"   . nvp-align-cols)
  ("c"   . align-current)
  ("Dh"  . align-highlight-rule)
  ("Du"  . align-unhighlight-rule)
  ("e"   . align-entire)
  ("M-?" . nvp-align-show-rules)
  (";"   . nvp-align-comments)
  ("r"   . align-repeat))

(mapc
 (lambda (key)
   (define-key nvp-align-keymap `[,key] #'nvp-align-by-last-char))
 (string-to-list "#=.,|\\"))

;;;###autoload
(defun nvp-align (&optional arg beg end)
  "Align buffer region b/w BEG and END, or call `nvp-mark-defun' if nil. 
(4) prefix, align entire active region or buffer. 
(16) prefix, highlight changes that would occur."
  (interactive
   (cons (prefix-numeric-value current-prefix-arg)
         (or (nvp-tap-or-region 'bdwim (nvp-prefix 4 'buffer 'defun) :pulse t)
             (nvp-tap-or-region 'bdwim 'paragraph :pulse t))))
  (if (eq arg 16) (call-interactively 'align-highlight-rule)
    (indent-region beg end)
    (align beg end)))

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
                 (list (nvp-input 'lcs) beg end)))
  (let ((re (concat "\\(\\s-+\\)" (regexp-quote char)
                    (if (or current-prefix-arg (string= char "\\")) "$" ""))))
    (align-regexp beg end re)))

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

;; -------------------------------------------------------------------
;;; Alignment rules

;; `align-exclude-rules-list'
;; `align-rules-list'

;; use a variable for modes that align '\' line continuations
(defvar nvp-align-basic-lc-modes
  (eval
   (cdr (assq 'modes (assq 'basic-line-continuation align-rules-list)))))

(setf
 (cdr (assq 'modes (assq 'basic-line-continuation align-rules-list)))
 nvp-align-basic-lc-modes)

(defvar nvp-align--groups
  '(;; All predefined mode groupings
    align-dq-string-modes align-sq-string-modes align-open-comment-modes
    align-c++-modes align-perl-modes align-lisp-modes align-tex-modes
    ;; plus mine
    nvp-align-basic-lc-modes
    ))

;; Collect align/exclude rules for MODE
(defun nvp-align--mode-rules (&optional mode)
  (nvp-defq mode major-mode)
  (--map
   (--filter
    (--> (eval (cdr (assoc 'modes (cddr it))))
         (or (memq mode it) (apply #'provided-mode-derived-p mode it)))
    it)
   `(,align-rules-list ,align-exclude-rules-list)))

;;;###autoload
(defun nvp-align-show-rules (&optional mode)
  "Show align/exclude rules applicable to `major-mode' or MODE."
  (interactive (list (nvp-prefix 4 (intern (nvp-read-mode)) major-mode)))
  (nvp-defq mode major-mode)
  (-let (((rules excludes) (nvp-align--mode-rules mode))
         (groups (--filter (apply #'provided-mode-derived-p mode (eval it))
                           nvp-align--groups)))
    (nvp-with-results-buffer nil (format "Align rules for %s" mode)
      (princ ";;; Member groups\n")
      (pp groups)
      (princ "\n;;; Align Rules\n")
      (princ ";; ") (pp (--map (car it) rules))
      (pp rules)
      (princ "\n;;; Exclude Rules\n")
      (princ ";; ") (pp (--map (car it) excludes))
      (pp excludes)
      (princ "\n;;; All Groups")
      (dolist (group nvp-align--groups)
        (princ "\n;; ")
        (princ group)
        (cl-prettyprint (symbol-value group)))
      (emacs-lisp-mode))))


;; -------------------------------------------------------------------
;;; Mode rules
;; #<marker at 13440 in align.el>
;; FIXME:
;; Buggy rules:
;; - exc-dq-string
;;   - doesn't account for multiline strings
;;   - is fooled by escaped quotes
;; - exc-open-comment treats quoted comment starters as normal

;; better make macro regexp:  allow _ in macro names and '?='
(setf
 (cdr (assq 'regexp (assq 'make-assignment align-rules-list)))
 (concat "^\\s-*[[:alpha:]_][[:alnum:]_]*\\(\\s-*\\)[\?:]?="
         "\\(\\s-*\\)\\([^	\n \\]\\|$\\)"))

;; sh

;; makefile => align-open-comment-modes
;; rules: open-comment, make-assignment, basic-line-continuation
;; exclude: exc-open-comment
;; setf (assoc 'make-assignment align-rules-list)
;; (defun nvp-align-mode (mode)
;;   (interactive (list (nvp-prefix 4 (intern (nvp-read-mode)) major-mode)))
;;   ())

;; FIXME: these rules can be useful for other modes: makefile, automake, etc.
;; alignment rules to align '\' not in strings/comments and
;; align end-of-line comments
;; (defvar nvp-sh-align-rules-list
;;   `((sh-line-continuation
;;      (regexp . "\\(\\s-*\\)\\\\\\s-*$")
;;      (modes  . '(sh-mode))              ; abc
;;      (valid  . ,(function ; one two three
;;                  (lambda () (save-excursion
;;                          (not (sh-in-comment-or-string (point))))))))
;;     (sh-eol-comments
;;      (regexp . "[^ #\t\n\\\\]\\(\\s-+\\)#+.*$")
;;      (group  . 1)
;;      (modes  . '(sh-mode))
;;      (valid  . ,(function
;;                  (lambda () (save-excursion
;;                          (goto-char (match-beginning 1))
;;                          (and (not (bolp))
;;                               (not (nth 3 (syntax-ppss)))))))))))

(provide 'nvp-align)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-align.el ends here
