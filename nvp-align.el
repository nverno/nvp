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
(require 'transient)
(require 'nvp)
(require 'align)
(nvp:decls :f (nvp-read-mode cl-prettyprint))

(nvp:bindings nvp-align-keymap nil
  :create t
  ("<f2>" . nvp-align-menu)
  ("A"    . align)
  ("a"    . nvp-align)
  ("C"    . nvp-align-cols)
  ("c"    . align-current)
  ("Dh"   . align-highlight-rule)
  ("Du"   . align-unhighlight-rule)
  ("e"    . align-entire)
  ("M-?"  . nvp-align-show-rules)
  (";"    . nvp-align-comments)
  ("r"    . align-regexp)
  ("R"    . nvp-align-repeat))

(mapc
 (lambda (key)
   (define-key nvp-align-keymap `[,key] #'nvp-align-by-last-char))
 (string-to-list "#=.,|\\:"))

;;;###autoload(autoload 'nvp-align-menu "nvp-align" nil t)
(transient-define-prefix nvp-align-menu ()
  [["Align"
    ("a" "Align" nvp-align)
    ("c" "Current" align-current)
    ("C" "Columns" nvp-align-cols)
    (";" "Comments" nvp-align-comments)
    ("r" "Regexp" align-regexp)
    ("R" "Repeat" nvp-align-repeat)]
   ["Show"
    ("h" "Highlight rule" align-highlight-rule :transient t)
    ("H" "Un-highlight rule" align-unhighlight-rule :transient t)
    ("?" "Show mode rules" nvp-align-show-rules)]])

;;;###autoload
(defun nvp-align (&optional arg beg end)
  "Align buffer region b/w BEG and END, or call `nvp-mark-defun' if nil.
(4) prefix, align entire active region or buffer.
(16) prefix, highlight changes that would occur."
  (interactive
   (cons (prefix-numeric-value current-prefix-arg)
         (or (nvp:tap-or-region 'bdwim (nvp:prefix 4 'buffer 'defun) :pulse t)
             (nvp:tap-or-region 'bdwim 'paragraph :pulse t))))
  (if (eq arg 16) (call-interactively 'align-highlight-rule)
    (indent-region beg end)
    (align beg end)))

;; Align single end-of-line comments within marked regions.
;; Doesn't align if double-quote is found before end-of-line. Not robust,
;; better to use `align-mode-rules-list' to account for comments/strings
;;;###autoload
(defun nvp-align-comments (beg end)
  (interactive (nvp:with-region beg end 'paragraph :pulse t
                 (list beg end)))
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
  (interactive (nvp:with-region beg end 'paragraph :pulse t
                 (list (nvp:input 'lcs) beg end)))
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
  (nvp-align-repeat start end "\\(\"[^\"]+\"\\|\'[^\']+'\\|[^ ]+\\)" t)
  (indent-region start end))

;; -------------------------------------------------------------------
;;; Alignment rules
;;
;; `align-exclude-rules-list'
;; `align-rules-list'
;;
;; #<marker at 13440 in align.el>
;; Buggy rules:
;; - exc-dq-string
;;   - doesn't account for multiline strings
;;   - is fooled by escaped quotes
;; - exc-open-comment treats quoted comment starters as normal

;; Line continuation (EOL = '\') - modifies 'basic-line-continuation to:
;; - store modes in variable so it can be extended easily
;; - not align in string/comments
(defvar nvp-align-basic-lc-modes
  '(python-mode sh-mode makefile-mode dockerfile-mode))

;; EOL comments
(defvar nvp-align-eol-comment-modes
  '(sh-mode makefile-mode))

;; -------------------------------------------------------------------
;;; Add / modify default rules

(setcdr (assq 'basic-line-continuation align-rules-list)
        `((regexp . "\\(\\s-*\\)\\\\$")
          (modes  . nvp-align-basic-lc-modes)
          (valid  . ,(function (lambda () (not (nvp:ppss 'soc)))))))

(unless (assq 'basic-eol-comments align-rules-list)
  (push
   (list 'basic-eol-comments
         (cons 'regexp
               ;; not whitespace, escape char, or comment begin
               (concat (nvp:regex-complement (?- ?\\ ?<))
                       "\\(\\s-+\\)\\s<+.*$"))
         '(modes  . nvp-align-eol-comment-modes)
         '(group  . 1)
         (cons 'valid
               (function
                (lambda ()
                  (save-excursion
                    (goto-char (match-beginning 1))
                    (and (not (bolp))
                         (not (nvp:ppss 'soc))))))))
   align-rules-list))

;; don't align '.' in multi-line strings
(unless (assq 'valid (cdr (assq 'lisp-alist-dot align-rules-list)))
  (push (cons 'valid (function (lambda () (not (nvp:ppss 'soc)))))
        (cdr (assq 'lisp-alist-dot align-rules-list))))

;; better make macro regexp:  allow _ in macro names and '[-+?]='
(setf (cdr (assq 'regexp (assq 'make-assignment align-rules-list)))
      ;; careful not to mess with assignments in shell scripts
      ;; ie. ignores aligning any assignments prefixed with tabs
      (concat "^[ ]*[[:alpha:]_][[:alnum:]_]*\\(\\s-*?\\)[ ?:+-]?="
              "\\(\\s-*\\)\\([^	\n \\]\\|$\\)"))

;; -------------------------------------------------------------------
;;; Show rules

(defvar nvp-align--groups
  '(;; All predefined mode groupings
    align-dq-string-modes align-sq-string-modes align-open-comment-modes
    align-c++-modes align-perl-modes align-lisp-modes align-tex-modes
    ;; plus mine
    nvp-align-basic-lc-modes
    nvp-align-eol-comment-modes))

;; Collect align/exclude rules for MODE
(defun nvp-align--mode-rules (&optional mode)
  (or mode (setq mode major-mode))
  (--map (--filter (--> (eval (cdr (assoc 'modes (cddr it))))
                        (or (memq mode it)
                            (apply #'provided-mode-derived-p mode it)))
                   it)
         `(,align-rules-list ,align-exclude-rules-list)))

;;;###autoload
(defun nvp-align-show-rules (&optional mode)
  "Show align/exclude rules applicable to `major-mode' or MODE."
  (interactive (list (nvp:prefix 4 (intern (nvp-read-mode)) major-mode)))
  (or mode (setq mode major-mode))
  (-let (((rules excludes) (nvp-align--mode-rules mode))
         (groups (--filter (apply #'provided-mode-derived-p mode (eval it))
                           nvp-align--groups)))
    (nvp:with-results-buffer :title (format "Align rules for %s" mode)
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

(provide 'nvp-align)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-align.el ends here
