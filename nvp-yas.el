;;; nvp-yas.el --- snippet helpers -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; XXX: should the base snippets be compiled? They rarely change
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(eval-and-compile (require 'yasnippet))
(nvp:auto "nvp-parse" 'nvp-parse-current-function)
(nvp:auto "s" 's-upper-camel-case 's-snake-case 's-match 's-chop-prefix)

(defvaralias '$% 'yas-selected-text)
(nvp:decls :v (yas-moving-away-p yas-modified-p)
           :f (typescript--in-documentation-comment-p))

;;; Buffers/File names
(defsubst nvp-yas-bfn () (nvp:path 'bfs nil :or-name t))
(defsubst nvp-yas-bfn-no-ext () (nvp:path 'bfse nil :or-name t))
(defsubst nvp-yas-dfn () (nvp:path 'ds))
(defsubst nvp-yas-ext () (nvp:path 'ext))
(defsubst nvp-yas-indent () (current-indentation))

;;; Syntax
;; yas-inside-string uses `font-lock-string-face'
(defsubst nvp-yas-in-string () (nth 3 (syntax-ppss)))
(defsubst nvp-yas-in-comment () (nth 4 (syntax-ppss)))
(defsubst nvp-yas-in-soc () (nvp:ppss 'soc))

;; inside doc strings
(defsubst nvp-yas-in-doc ()
  (or (memq 'font-lock-doc-face (nvp:as-list (get-text-property (point) 'face)))
      (and (eq major-mode 'typescript-mode)
           (typescript--in-documentation-comment-p))))

;; Or patterns
(defsubst nvp-yas-or-values (str &optional seps)
  (split-string str (or seps "[\]\[|]") 'omit " "))

;; -------------------------------------------------------------------
;;; Docstrings

;; curly, html
;; (nvp:defvar :local t :permanent t nvp-docstring-style 'curly "Docstring style")

;; (defun nvp-yas-ds-open (tag)
;;   (pcase nvp-docstring-style
;;     ('html (concat "<" tag ">"))
;;     (_ (concat "{@" tag))))

;; (defun nvp-yas-ds-close (tag)
;;   (pcase nvp-docstring-style
;;     ('html (concat "</" tag ">"))
;;     (_ "}")))

;; (defalias '${ 'nvp-yas-ds-open)
;; (defalias '$} 'nvp-yas-ds-close)

;; -------------------------------------------------------------------
;;; Expansion helpers
;; see doom-snippets.el

(defun nvp-yas-count-lines (str)
  (if (and (stringp str)
           (not (string-empty-p str)))
      (length (split-string str "\\(\r\n\\|[\r\n]\\)"))
    0))

;; from doom-snippets-lib.el
(defun nvp-yas-bolp ()
  "Return t if point is preceded only by indentation.
Unlike `bolp', this ignores the trigger word for the current snippet."
  (or (bolp)
      (save-excursion
        (if (region-active-p)
            (goto-char (region-beginning))
          (unless (memq (char-before) (list ?\  ?\n))
            (backward-word)))
        (skip-chars-backward " \t")
        (bolp))))

(defun nvp-yas-format (format &optional default trim)
  "Returns formatted string (see `doom-snippets-format').
If TRIM is non-nil, whitespace is removed from selected text.

  %s  `yas-selected-text' or DEFAULT
  %n  A newline, if current selection spans more than a single line
  %e  A newline, unless the point is at EOL"
  (let* ((text (or yas-selected-text default ""))
         (text (if trim (string-trim text) text)))
    (format-spec format
                 `((?s . ,text)
                   (?n . ,(if (> (nvp-yas-count-lines text) 1) "\n" ""))
                   (?e . ,(if (eolp) "" "\n"))))))

(defvaralias '$tmp 'nvp--yas-tmp)
(defvar nvp--yas-tmp nil)
(defun nvp-yas-inactive-p () (null yas--active-snippets))
(defun nvp-yas-set-tmp (expr) (progn (setq nvp--yas-tmp (eval expr)) ""))
(defun nvp-yas-newline-selected-newline () (nvp-yas-format "%n%s%n" nil t))
(defun nvp-yas-newline-selected () (nvp-yas-format "%s%n" nil t))
(defun nvp-yas-newline-or-eol () (nvp-yas-format "%e"))
(defun nvp-yas-semi () (if (eolp) ";"))
(defun nvp-yas-string= (exp res) (if (string= exp (yas-text)) res ""))
(defun nvp-yas-string!= (exp res) (if (string= exp (yas-text)) "" res))
(defun nvp-yas-string? (res) (and (yas-text) res))
(defalias '$!%! 'nvp-yas-newline-selected-newline)
(defalias '$!% 'nvp-yas-newline-selected)
(defalias '$$ 'nvp-yas-newline-or-eol)
(defalias '$format 'nvp-yas-format)
(defalias '$> 'nvp-yas-semi)
(defalias '$set 'nvp-yas-set-tmp)
(defalias '$s= 'nvp-yas-string=)
(defalias '$s!= 'nvp-yas-string!=)
(defalias '$s? 'nvp-yas-string?)

;;; -------------------------------------------------------------------
;; Comments

(defalias 'yas-comment-string 'nvp-yas-comment)

(defun nvp-comment-string (str &optional padlen)
  "Wrap STR with modes starting and ending comment delimiters.
If PADLEN is non-nil, start with PADLEN comment starters."
  (let* ((ctrim (string-trim-right comment-start))
         (comment (if (or (derived-mode-p 'c-mode)
                          (memq major-mode '(python-mode))
                          (string= comment-end ""))
                      ctrim
                    (concat ctrim ctrim))))
    (when (and padlen (> padlen (length comment)))
      (setq comment (nvp-comment-start padlen comment)))
    (format "%s %s%s" comment str comment-end)))

(defun nvp-comment-start (length &optional start)
  "Create comment string of LENGTH starting with `comment-start' or START.
Accounts for multi-character comments by recycling the second character."
  (let* ((comment (or start (string-trim-right comment-start)))
         (cont (if (> (length comment) 1) (aref comment 1) (aref comment 0))))
    (concat comment (make-string (max 0 (- length (length comment))) cont))))

(defun nvp-comment-continued (length)
  "Make a comment continuation with LENGTH padding concated with `comment-end'."
  (if (and comment-end (not (string= "" comment-end)))
      (if (> (length comment-start) 1)
          (concat (make-string (1- length) ? ) (substring comment-start 1 2))
        (make-string length ? ))
    (nvp-comment-start length)))

(defun nvp-comment-end (&optional trim)
  "Return a TRIMmed version `comment-end' or \"\" if not defined."
  (if (bound-and-true-p comment-end)
      (if trim (string-trim comment-end) comment-end)
    ""))

;; -------------------------------------------------------------------
;;; Padding / Headers

;; create string length of `yas-text', optionally constrained by min-len/max-len
(defsubst nvp-yas-header (char &optional min-len max-len)
  (let ((sw (+ (or min-len 0) (string-width yas-text))))
    (make-string (if max-len (max 0 (min sw (- max-len sw))) sw) char)))

;; add padding to `yas-text'
(defsubst nvp-yas-pad (char padmin padmax)
  (let* ((sw (+ padmin (string-width yas-text)))
         (extra (max 0 sw (- padmax sw))))
    (make-string (/ extra 2) char)))

;; fill after yas-text with CHAR until PADMAX
(defsubst nvp-yas-pad-right (char padmax)
  (make-string (max 0 (- padmax (string-width yas-text))) char))

;; center `yas-text' constrained by padmin/padmax
(defsubst nvp-yas-center (padmin padmax &optional char)
  (or char (setq char ? ))
  (let* ((sw (+ padmin (length yas-text)))
         (extra (max 0 sw (- padmax sw))))
    (concat (make-string (ceiling extra 2) char)
            yas-text
            (make-string (floor extra 2) char))))


;; -------------------------------------------------------------------
;;; Functions, args, variables

;; Function arguments w/o types
;; split argument STR by SEPS, eg. "a,b" => '("a" "b"). Strings are trimmed and
;; nulls are dropped.
;; if DEFAULTS is non-nil, split by "=" as well, eg.
;; "a,b=1," => '("a" ("b" "1"))
(defun nvp-yas-split-args (str &optional seps defaults)
  (let ((args (split-string str (or seps "[ \t]*,[ \t]*") t " ")))
    (if defaults
        (mapcar (lambda (s)
                  (let ((defs (split-string s "[ \t]*=[ \t]*" t " ")))
                    (if (= (length defs) 1) (car defs) defs)))
                args)
      args)))

;; Variable that may/may not have a type, eg in a for loop
;; Gets variable name from string of form "i = 1" or "int i = 1"
(defun nvp-yas-var (str)
  (if (< (length str) 1)
      ""
   (let* ((str (car (split-string str "=" t " ")))
          (strs (split-string str nil t " ")))
     (or (cadr strs) (car strs)))))

;; name of current function or script
(defun nvp-yas-function-or-script ()
  (or (nvp-parse-current-function)
      (nvp:path 'bfse nil :or-name t)))

;; build param strings: params range from BEG length LEN
;; each param is prepended by JOIN string
(defsubst nvp-yas-param-str (beg len join &optional fmt)
  (or fmt (setq fmt "$%d"))
  (if (or (not len) (= len 0)) ""
    (concat join
            (mapconcat (lambda (n) (format fmt n))
                       (number-sequence beg (+ beg (1- len))) join))))


;;; Input

;; yas read input wrapper
(defun nvp-yas-read (func &rest args)
  (unless (or yas-moving-away-p
              yas-modified-p)
    (apply func args)))

(provide 'nvp-yas)
;;; nvp-yas.el ends here
