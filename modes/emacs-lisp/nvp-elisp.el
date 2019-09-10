;;; nvp-elisp.el --- elisp helpers  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'pp)
(require 'company-elisp)
(require 'nvp-parse)
(nvp-decls)
(nvp-decl company-elisp--candidates-predicate company-elisp--fns-regexp)

;; -------------------------------------------------------------------
;;; Util

(eval-when-compile
  (defvar nvp-elisp-defuns-regexp)
  (defvar nvp-elisp-var-binding-regexp))

;; modified from company-elisp to incorporate more things
;; used to determine abbrev expansion / toggling
(let-when-compile
    ((el-defs '("defun" "defmacro" "defsubst" "defmethod" "defclass" "defgeneric"
                "define-advice" "defadvice" "add-advice" "add-function"
                ))
     (el-vdefs '("let" "cond" "lexical-let" "if-let" "when-let" "lambda"
                 "labels" "flet"
                 ;; "pcase"
                 "pcase-dolist" "pcase-lambda"
                 "pcase-exhaustive" "pcase-let")))
  (let ((vars-re
         (eval-when-compile
           (apply #'company-elisp--fns-regexp (append el-defs el-vdefs))))
        (defs-re
          (eval-when-compile
            (concat "([ \t\n]*" (apply #'company-elisp--fns-regexp el-defs)))))
    ;; include generics
    (defvar nvp-elisp-defuns-regexp defs-re)
    ;; additional let macros, pcase, cond, etc.
    (defvar nvp-elisp-var-binding-regexp vars-re)))

;; -------------------------------------------------------------------
;;; Things at point

(defun nvp-elisp-bounds-of-cons ()
  "Return bounds of dotted cons, eg (sexp . sexp)."
  (save-excursion
    (let* ((syntax (nvp-ppss 'partial))
           (parens (reverse (nth 9 syntax))))
      (cl-block nil
        (dolist (pos parens)
          (goto-char (1+ pos))
          (forward-sexp)
          (skip-chars-forward "^.)")
          (and (eq ?. (char-after))
               (cl-return (bounds-of-thing-at-point 'list))))))))

(put 'cons 'bounds-of-thing-at-point 'nvp-elisp-bounds-of-cons)

(defun nvp-elisp-bounds-of-alist ()
  "Return bounds of alist at point.
Also returns bounds of type (some-macro (&rest args) (a . b) (c . d) ...)."
  (cl-block nil
    (if (memq ?\' (list (char-before) (char-after)))
        (and (nvp-goto 'fdl)
             (bounds-of-thing-at-point 'list))
      (save-excursion
        (while (nvp-goto 'bul)         ;search backward up lists for a '(
          (and (eq (char-before) ?\')
               (nvp-goto 'fdl)
               (cl-return (bounds-of-thing-at-point 'list)))))
      ;; check if in cons-cell and back out of it
      ;; eg. (macro (foo . bar) (goo . ber))
      (save-excursion
        (when-let ((bnds (nvp-tap 'btap 'cons)))
          (goto-char (1- (car bnds)))
          (bounds-of-thing-at-point 'list))))))

(put 'alist 'bounds-of-thing-at-point 'nvp-elisp-bounds-of-alist)

;; -------------------------------------------------------------------
;;; Generics

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Where-Defined.html
(defvar nvp-elisp--defun-forms
  '(defun defmacro defsubst cl-defun cl-defsubst cl-defmacro declare-function t
    autoload cl-defmethod)
  "Forms to recognize for function names.")

;; return forms defined in FILENAME
(defsubst nvp-elisp--file-forms (filename)
  (cl-assoc (regexp-quote filename) load-history :test #'string-match-p))

;; collect forms whose car is a member of ELEMS
(defsubst nvp-elisp--filter-forms (elems filename)
  (cl-loop for elt in (nvp-elisp--file-forms filename)
     when (and (consp elt) (memq (car elt) elems))
     collect (if (consp (cdr elt)) (cadr elt) (cdr elt))))

(eval-when-compile
  (defmacro nvp-elisp:get-forms (elems args)
    "Filter matching ELEMS from file's forms (possibly loading file).
Forms are read from :file if present in ARGS, otherwise current buffer file."
    (declare (indent defun) (debug t))
    (nvp-with-syms (fname buff lib pargs)
      `(-let* ((,pargs ,args)
               ((&plist :file ,fname :buffer ,buff :library ,lib) ,pargs))
         (if ,buff
             (with-current-buffer ,buff
               (nvp-elisp--filter-forms ,elems (buffer-file-name)))
           (when (plist-get ,pargs :do-load)
             (and ,fname (load-file ,fname))
             (and ,lib (require ,lib ,fname t)))
           (and ,lib (setq ,fname (file-name-sans-extension (locate-library ,lib))))
           (nvp-elisp--filter-forms ,elems (or ,fname (buffer-file-name))))))))

;; try(not very hard) to gather buffer functions/macros at top level
;; from current buffer, optionally in region specified by BEG END
(defun nvp-elisp-matching-forms (match-forms &optional beg end)
  (or end (setq end (point-max)))
  (save-excursion
    (let (forms form)
      (goto-char (or beg (point-min)))
      (ignore-errors
        (while (and (setq form (read (current-buffer)))
                    (< (point) end))
          (when (and (consp form) (memq (car form) match-forms))
            (let ((sym (cadr form)))
              (push (if (eq 'quote (car-safe sym)) (cadr sym) sym) forms)))))
      (delq nil forms))))

(cl-defmethod nvp-parse-functions
  (&context (major-mode emacs-lisp-mode) &rest args)
  "Accepts additional ARGS, :do-load to `load-file' prior to parsing."
  (or (nvp-elisp:get-forms nvp-elisp--defun-forms args)
      ;; gather functions from unloaded buffer / file
      (nvp-parse:buffer-file t nil args
        (nvp-elisp-matching-forms nvp-elisp--defun-forms))))

(cl-defmethod nvp-parse-current-function
  (&context (major-mode emacs-lisp-mode) &rest _args)
  (or (add-log-current-defun) (cl-call-next-method)))

;; file could have multiple provides
(cl-defmethod nvp-parse-library
  (&context (major-mode emacs-lisp-mode) &rest args)
  (-when-let (libs (or (nvp-elisp:get-forms '(provide) args)
                       (nvp-parse:buffer-file t nil args
                         (nvp-elisp-matching-forms '(provide)))))
    (if (= 1 (length libs)) (car libs) libs)))

(cl-defmethod nvp-parse-includes
  (&context (major-mode emacs-lisp-mode) &rest args)
  (nvp-elisp:get-forms '(require) args))

;; ------------------------------------------------------------
;;; Eval

;; Eval region from BEG to END if active, otherwise the last sexp.
(defun nvp-elisp-eval-last-sexp-or-region (arg)
  "Eval region if active, otherwise last sexp.
If in `lisp-interaction-mode' or with prefix ARG, pretty-print the results."
  (interactive "P")
  (let ((expr
         (if (and (mark) (use-region-p))
             (let ((beg (min (point) (mark)))
                   (end (max (point) (mark))))
               (save-restriction
                 (narrow-to-region beg end)
                 (read (current-buffer))))
           (pp-last-sexp)))
        (print-length (window-total-height))
        (print-level)
        (print-circle t)
        (debug-on-error t))
    (if arg (pp-eval-expression expr)
      (if (eq 'lisp-interaction-mode major-mode)
          (let ((standard-output (current-buffer)))
            (cl-prettyprint (eval expr)))
        (eval-expression expr)))))

;; Jump to end of line and try eval if not looking back at `)'.
(defun nvp-elisp-eval-last-sexp-dwim (arg)
  "Eval last sexp if directly after one, otherwise skip forward up list or sexp.
ARG is passed to `nvp-elisp-eval-last-sexp-or-region'."
  (interactive "P")
  (if (not (looking-back "\\s-*)" (line-beginning-position)))
      (or (nvp-goto 'ful) (forward-sexp))) ; forward up list or sexp
  (nvp-elisp-eval-last-sexp-or-region arg))

(defun nvp-elisp-eval-and-replace ()
  "Replace preceding sexp with its evaluated value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
	     (current-buffer))
    (error (message "invalid expression")
	   (insert (current-kill 0)))))

;; -------------------------------------------------------------------
;;; Indentation

;; cl-flet etc. indentation
(with-eval-after-load 'cl-indent
  (let ((indent (get 'flet 'common-lisp-indent-function)))
    (mapc #'(lambda (f) (put f 'common-lisp-indent-function indent))
          '(cl-flet cl-flet* labels cl-labels cl-macrolet))))
(put 'cl-loop 'lisp-indent-function 'common-lisp-indent-function)
(put 'cl-labels 'lisp-indent-function 'common-lisp-indent-function)
(put 'lisp-indent-function 'safe-local-variable 'symbolp)

;; -------------------------------------------------------------------
;;; Insert / Toggle 

;; indent region with common-lisp-indent function
;; with prefix toggle b/w lisp-indent and common-lisp-indent
(defun nvp-elisp-toggle-cl-indent (&optional start end switch)
  "Indent region with `common-lisp-indent-function'.
With \\[universal-argument] toggle buffer-local `common-lisp-indent-function'."
  (interactive "r\nP")
  (when switch
    (setq-local lisp-indent-function
                (if (eq lisp-indent-function 'lisp-indent-function)
                    'common-lisp-indent-function
                  'lisp-indent-function)))
  (let ((lisp-indent-function 'common-lisp-indent-function))
    (put 'cl-flet 'common-lisp-indent-function
         (get 'flet 'common-lisp-indent-function))
    (indent-region start end)))

(defun nvp-elisp-toggle-lexical ()
  "Toggle `lexical-binding' on/off."
  (interactive)
  (require 'nvp-toggle)
  (nvp-toggle-local-variable 'lexical-binding t))

(defun nvp-elisp-toggle-auto ()
  "Toggle autoload on/off when in or directly before sexp.
If in function definition, toggle autoload cookie.
If in `declare-function', convert to autoload."
  (interactive)
  (save-match-data
    (save-excursion
      (ignore-errors (goto-char (car (nth 9 (syntax-ppss)))))
      (cond
       ((looking-at "(declare-function\\s-+")
        (replace-match "(autoload '"))
       ((looking-at "(autoload\\s-+'")
        (replace-match "(declare-function "))
       ((looking-at nvp-elisp-defuns-regexp)
        (and (bobp) (insert "\n"))
        (forward-line -1)
        (if (looking-at ";;;###autoload[ \t]*\n")
            (replace-match "")
          (unless (looking-at-p "^\\s-*$")
            (end-of-line))
          (insert "\n;;;###autoload")))
       (t (message "Location not autoloadable"))))))

;; ------------------------------------------------------------
;;; Abbrevs: expansion predicates

;; don't expand when prefixed by '-'
(defvar nvp-lisp-abbrev-re "\\(\\_<[_:\\.A-Za-z0-9/=<>]+\\)")

;; Don't expand in strings, comments, function args, let bindings or after '-/'
;; Note: Could also account for dolist/dotimes/etc
(eval-when-compile
  (defvar company-elisp-var-binding-regexp)
  (defmacro nvp-elisp:abbrev-expand-p (type)
    (declare (debug t))
    `(and (or (memq this-command '(expand-abbrev nvp-abbrev-expand-after-symbols))
              (not (memq last-input-event '(?/ ?- ?= ?> ?<))))
          (not (or (nvp-ppss 'soc)      ; in string or comment
                   (let ((company-elisp-var-binding-regexp
                          nvp-elisp-var-binding-regexp))
                     (eq (company-elisp--candidates-predicate (nvp-abbrev-grab))
                         ,type)))))))

;; function expansion
(defun nvp-elisp-abbrev-expand-fn-p ()
  (nvp-elisp:abbrev-expand-p 'boundp))

;; variable expansion
(defun nvp-elisp-abbrev-expand-var-p ()
  (nvp-elisp:abbrev-expand-p 'fboundp))

;; ------------------------------------------------------------
;;; Advices

;; use `pop-to-buffer' and set local `ielm-working-buffer'
(define-advice ielm (:around (orig-fn &rest _args) "pop-to-buffer")
  (let ((orig-buff (current-buffer)))
   (with-current-buffer (get-buffer-create "*ielm*")
     (nvp-with-letf #'pop-to-buffer-same-window #'ignore
       (funcall orig-fn))
     (prog1 (current-buffer)
       (setq-local ielm-working-buffer orig-buff)
       (and current-prefix-arg (pop-to-buffer (current-buffer)))))))

;; -------------------------------------------------------------------
;;; Compile

;; (defun nvp-elisp-compile ()
;;   (interactive)
;;   (nvp-compile-with-bindings
;;    `([remap recompile] . 
;;      (lambda ()
;;        (interactive)
;;        (with-current-buffer ,(current-buffer)
;;          (emacs-lisp-byte-compile))))))

;; ------------------------------------------------------------
;;; Imenu

(eval-and-compile
  (nvp-setq nvp-elisp-imenu-headers
            (let* ((prefix "^;;\\(?:;\\{1,2\\}\\|[*]\\{1,2\\}\\| |\\)\\s-+")
                   (hdr-regex (concat prefix "\\([^#;].*\\)\\s-*$"))
                   (pkg-hdrs
                    (concat prefix "\\("
                            (regexp-opt '("Commentary:" "Code:" "Documentation:"
                                          "History:" "ChangeLog:" "Change Log:"))
                            "\\|.*\\.el\\)")))
              ;; don't include default package headers, beginning/end of file
              `((nil ,(macroexpand-all
                       (lambda ()
                         (nvp-awhile (and (not (bobp))
                                          (re-search-backward hdr-regex nil t))
                           (unless (looking-at-p pkg-hdrs)
                             (cl-return t)))))
                     1))))
  
  (nvp-setq
    nvp-elisp-imenu-headers-1
    `(("Headers" ,(cadar nvp-elisp-imenu-headers) 1)
      ("Libs" "^;;\\s-*[*]\\s-*\\(?:[Ll]ibs?\\):\\s-*\\([[:alnum:]- /]+\\)" 1)))

  (defvar nvp-elisp-imenu-headers-2
    '(("Sub-Headers" "^;;---*\\s-*\\([^-\n]+\\)\\s-*-*$" 1))))

(provide 'nvp-elisp)
;;; nvp-elisp.el ends here
