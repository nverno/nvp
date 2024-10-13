;;; nvp-elisp.el --- Elisp -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-parse))
(require 'pp)
(require 'company-elisp) ; XXX(3/5/24): `company-elisp' removed from `company-mode'
(nvp:decls)


(with-eval-after-load 'nvp-repl
  (require 'nvp-ielm))

;; Modified from company-elisp to incorporate more things
;; used to determine abbrev expansion / toggling
(let-when-compile
    ((el-defs '("defun" "defmacro" "defsubst" "defmethod" "defclass" "defgeneric"
                "define-advice" "defadvice" "add-advice" "add-function"))
     (el-vdefs '("let" "cond" "lexical-let" "if-let" "when-let" "lambda"
                 "labels" "flet"
                 "pcase-dolist" "pcase-lambda" "pcase-exhaustive" "pcase-let")))
  (let ((vars-re (eval-when-compile
                   (apply #'company-elisp--fns-regexp (append el-defs el-vdefs))))
        (defs-re (eval-when-compile
                   (concat "([ \t\n]*" (apply #'company-elisp--fns-regexp el-defs)))))
    ;; Include generics
    (defvar nvp-elisp-defuns-regexp defs-re)
    ;; Additional let macros, pcase, cond, etc.
    (defvar nvp-elisp-var-binding-regexp vars-re)))

;; Note: lisp-data-mode is parent of emacs-lisp-mode so
;; (put 'derived-parent-mode ...) results in infinite loop
(defun nvp-company-lisp-data (&rest args)
  "Wrapper for `company-complete' in `lisp-data-mode'."
  (interactive (list 'interactive) lisp-data-mode)
  (let* ((orig-fn (symbol-function 'company-elisp))
         (major-mode 'emacs-lisp-mode))
    (apply orig-fn args)))


;; -------------------------------------------------------------------
;;; Things at point

(defun nvp-elisp-bounds-of-symbol-at-point ()
  "In code, ignore \"@\" prefix from symbols.
In strings, ignore doc comment prefixes/suffixes that confuse xref."
  (when-let ((bnds (bounds-of-thing-at-point 'symbol)))
    (let* ((str-p (nvp:ppss 'str))
           (pre (if str-p '(?{ ?<) '(?@)))
           (post (if str-p '(?} ?>))))
      (while (and (< (car bnds) (cdr bnds))
                  (memq (char-after (car bnds)) pre))
        (setf (car bnds) (1+ (car bnds))))
      (while (and post
                  (< (car bnds) (cdr bnds))
                  (memq (char-before (cdr bnds)) post))
        (setf (cdr bnds) (1- (cdr bnds)))))
    bnds))
(put 'elisp-symbol 'bounds-of-thing-at-point
     'nvp-elisp-bounds-of-symbol-at-point)

(defun nvp-elisp-symbol-at-point ()
  "For emacs-lisp `thing-at-point-provider-alist'."
  (thing-at-point 'elisp-symbol))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql 'elisp)))
  "Override default to use \\='elisp-symbol."
  (--when-let (bounds-of-thing-at-point 'elisp-symbol)
    (let ((ident (buffer-substring-no-properties (car it) (cdr it))))
      ;; Use a property to transport the location of the identifier.
      (propertize ident 'pos (car it)))))

;; Dont include prefix '@' in iedit
(defvar nvp-elisp-iedit-syntax
  (let ((tab (copy-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?@ "'" tab)
    tab))

(defun nvp-elisp-bounds-of-cons ()
  "Return bounds of dotted cons, eg (sexp . sexp)."
  (save-excursion
    (let* ((syntax (parse-partial-sexp (point-min) (point)))
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
        (and (nvp:goto 'fdl)
             (bounds-of-thing-at-point 'list))
      (save-excursion
        (while (nvp:goto 'bul)         ;search backward up lists for a '(
          (and (eq (char-before) ?\')
               (nvp:goto 'fdl)
               (cl-return (bounds-of-thing-at-point 'list)))))
      ;; check if in cons-cell and back out of it
      ;; eg. (macro (foo . bar) (goo . ber))
      (save-excursion
        (when-let ((bnds (nvp:tap 'btap 'cons)))
          (goto-char (1- (car bnds)))
          (bounds-of-thing-at-point 'list))))))
(put 'alist 'bounds-of-thing-at-point 'nvp-elisp-bounds-of-alist)


;; -------------------------------------------------------------------
;;; Generics

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Where-Defined.html
(defvar nvp-elisp--defun-forms
  '( defun defmacro defsubst
     autoload declare-function t
     cl-defun cl-defsubst cl-defmacro cl-defmethod cl-defgeneric)
  "Forms to recognize for function names.")

(defsubst nvp-elisp--file-forms (filename)
  "Return forms defined in FILENAME from `load-history'."
  (cl-assoc (regexp-quote filename) load-history
            ;; Note: can be entry '(nil ...) in `load-history' from `eval-defun'
            ;; in unloaded file, eg. in scratch buffer.
            :key (lambda (elem) (or elem regexp-unmatchable))
            :test #'string-match-p))

(defsubst nvp-elisp--filter-forms (elems filename)
  "Collect forms loaded from FILENAME whose car is a member of ELEMS."
  (cl-loop for elt in (nvp-elisp--file-forms filename)
           when (and (consp elt) (memq (car elt) elems))
           collect (if (consp (cdr elt)) (cadr elt) (cdr elt))))

(defun nvp-elisp--get-forms (elems &optional args)
  "Filter matching ELEMS from file's forms (possibly loading file).
Forms are read from :file if present in ARGS, otherwise current buffer file."
  (-let* ((pargs args) ((&plist :file fname :buffer buff :library lib) pargs))
    (if buff (with-current-buffer buff
               (nvp-elisp--filter-forms elems (buffer-file-name)))
      (when (plist-get pargs :do-load)
        (and fname (load-file fname))
        (and lib (require lib fname t)))
      (and lib (setq fname (file-name-sans-extension (locate-library lib))))
      (nvp-elisp--filter-forms
       elems (or fname (file-name-sans-extension (buffer-file-name)))))))

(defun nvp-elisp-matching-forms (match-forms &optional beg end)
  "Gather (most of) current buffer's MATCH-FORMS at top-level from BEG to END."
  (or end (setq end (point-max)))
  (save-excursion
    (let (forms form)
      (goto-char (or beg (point-min)))
      (ignore-errors
        (while (and (setq form (read (current-buffer)))
                    (< (point) end))
          (pcase match-forms
            (`t (push form forms))
            ((pred functionp)
             (-when-let (res (funcall match-forms form))
               (push res forms)))
            ((pred (and (consp form) (memq (car form) match-forms)))
             (let ((sym (cadr form)))
               (push (if (eq 'quote (car-safe sym)) (cadr sym) sym) forms))))))
      (delq nil forms))))

(cl-defmethod nvp-parse-functions ((_mode (eql emacs-lisp-mode)) &rest args)
  "Accepts additional ARGS, :do-load to `load-file' prior to parsing."
  (or (nvp-elisp--get-forms nvp-elisp--defun-forms args)
      ;; Gather functions from unloaded buffer / file
      (nvp-parse:buffer-file t nil args
        (nvp-elisp-matching-forms nvp-elisp--defun-forms))))

(cl-defmethod nvp-parse-variables
  ((_mode (eql emacs-lisp-mode)) &rest args)
  "Return variables defined in buffer or file."
  (nvp-parse:buffer-file t nil args
    (nvp-elisp-matching-forms '(defvar defcustom))))

(cl-defmethod nvp-parse-current-function
  (&context (major-mode emacs-lisp-mode) &rest _args)
  (require 'nvp-parse)
  (or (add-log-current-defun)
      (cl-call-next-method)))

;; File could have multiple provides
(cl-defmethod nvp-parse-library ((_mode (eql emacs-lisp-mode)) &rest args)
  "Find provides from buffer or file."
  (or (nvp-elisp--get-forms '(provide) args)
      (nvp-parse:buffer-file t nil args
        (nvp-elisp-matching-forms '(provide)))))

(cl-defmethod nvp-parse-includes ((_mode (eql emacs-lisp-mode)) &rest args)
  "Find requires in buffer or file."
  (or (nvp-elisp--get-forms '(require) args)
      (nvp-parse:buffer-file t nil args
        (nvp-elisp-matching-forms '(require)))))


;; ------------------------------------------------------------
;;; Eval

(defun nvp-elisp-eval-and-replace (&optional beg end)
  "Evaluate region from BEG to END or preceding sexp and replace with reaults."
  (interactive (and (region-active-p)
                    (list (region-beginning) (region-end))))
  (if (and beg end)
      (kill-region beg end)
    (backward-kill-sexp))
  (condition-case err
      (pp (eval (read (current-kill 0)) lexical-binding)
          (current-buffer))
    (error (insert (current-kill 0))
           (user-error (error-message-string err)))))

(defun nvp-elisp-eval-print-last-sexp (arg)
  "Wrap `eval-print-last-sexp' so `C-u' ARG prints without truncation."
  (interactive
   (list (if (equal '(4) current-prefix-arg) 0 current-prefix-arg)))
  (funcall-interactively #'eval-print-last-sexp arg))

(defun nvp-elisp-eval-sexp-dwim (&optional no-truncate action newline and-go)
  "Eval active region, top-level sexp containing point, or the previous sexp.
Interactively, the prefix argument means:

   Prefix     ACTION
   ------     ------
   `-1'       \\='replace   Replace sexp with untruncated results.
   `-', 4     \\='pp        Pretty print result in temp buffer.
    <-1, >=16 \\='insert    Insert result at point or after evaluated sexp.
    *         \\='eval      Eval and echo result with `eval-last-sexp'.

   `-', <1               No truncation of results.

When ACTION is \\='insert, when:
  NEWLINE                 Insert newline before results.
  Interactive or AND-GO   Move point to end of evaluated sexp."
  (interactive (let* ((raw (prefix-numeric-value current-prefix-arg))
                      (arg (abs raw))
                      (action (cond ((eq -1 raw) 'replace)
                                    ((>= arg 16) 'pp)
                                    (t (if (memq raw '(- -4 4))
                                           'insert
                                         'eval))))
                      (no-truncate (or (eq '- raw) (<= raw 0)))
                      (and-go (eq 'insert action)))
                 (list no-truncate action nil and-go)))
  (let* ((print-level (unless no-truncate eval-expression-print-level))
         (print-length (unless no-truncate eval-expression-print-length))
         (debug-on-error eval-expression-debug-on-error)
         (region-p (region-active-p))
         (beg (if region-p (region-beginning)
                ;; Beginning of outermost sexp
                (car-safe (nth 9 (parse-partial-sexp (point-min) (point))))))
         (end (and region-p (region-end))))
    (save-excursion
      (when (and beg (null region-p))
        (goto-char beg)
        (forward-sexp)
        (setq end (point))
        (nvp-indicate-pulse-region-or-line beg end))
      (pcase-exhaustive action
        ('replace (funcall-interactively #'nvp-elisp-eval-and-replace beg end))
        ((or 'eval 'insert)
         (let* ((insert-p (eq 'insert action))
                (standard-output (if insert-p (current-buffer) t)))
           (and insert-p newline (terpri))
           (if region-p
               (eval-region beg end (and insert-p (current-buffer)))
             ;; Note(09/16/24): in lisp interaction, could use
             ;; (pp-to-string (eval expr lexical-binding))
             (eval-last-sexp (or (and (null insert-p) '-)
                                 (and no-truncate 0) t)))
           (and insert-p newline (terpri))))
        ('pp (let ((exp (if (null beg)
                            (pp-last-sexp)
                          (save-mark-and-excursion
                            (save-restriction
                              (narrow-to-region beg end)
                              (goto-char beg)
                              (read (current-buffer)))))))
               (unless exp
                 (user-error "didnt read a sexxp"))
               (nvp-eval--display-expression (eval exp lexical-binding)))))
      (setq end (point)))
    (when (and and-go end (not (eq 'replace action)))
      (goto-char end))))

(defun nvp-lisp-interaction-eval-dwim (prefix)
  "Eval sexp dwim in `lisp-interaction-mode'.
The PREFIX is interpreted as:

   `-1'         Replace
   `-', 0, 4    No-truncation
   `-', >=16    Pretty print evaluation in temp buffer
    *           Insert result in buffer after sexp and move point to end."
  (interactive "P")
  (let* ((raw (prefix-numeric-value prefix))
         (arg (abs raw))
         (no-truncate (or (eq '- prefix) (memq arg '(0 4))))
         (action (cond ((or (eq '- prefix) (>= arg 16)) 'pp)
                       ((eq -1 raw) 'replace)
                       (t 'insert))))
    (funcall #'nvp-elisp-eval-sexp-dwim no-truncate action t t)))

;; -------------------------------------------------------------------
;;; Insert / Toggle

;; cl-flet etc. indentation
(with-eval-after-load 'cl-indent
  (let ((indent (get 'flet 'common-lisp-indent-function)))
    (mapc (lambda (f) (put f 'common-lisp-indent-function indent))
          '(cl-flet cl-flet* labels cl-labels cl-macrolet))))
;; (put 'cl-loop 'lisp-indent-function 'common-lisp-indent-function)
;; (put 'cl-labels 'lisp-indent-function 'common-lisp-indent-function)
(put 'lisp-indent-function 'safe-local-variable 'symbolp)

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
      (ignore-errors
        (goto-char (car (nth 9 (syntax-ppss)))))
      (cond ((looking-at "(declare-function\\s-+")
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


;; -------------------------------------------------------------------
;;; Abbrevs

;; Don't expand when prefixed by '-'
(defvar nvp-lisp-abbrev-re "\\(\\_<[_:\\.A-Za-z0-9/=<>]+\\)")

;; Don't expand in strings, comments, function args, let bindings or after '-/'
;; Note: Could also account for dolist/dotimes/etc
(defvar company-elisp-var-binding-regexp)
(defun nvp-elisp--abbrev-expand-p (type)
  (and (or (memq this-command '(expand-abbrev nvp-abbrev-expand-after-symbols))
           (not (memq last-input-event '(?/ ?- ?= ?> ?<))))
       (not (or (nvp:ppss 'soc)
                (let ((company-elisp-var-binding-regexp
                       nvp-elisp-var-binding-regexp))
                  (eq (company-elisp--candidates-predicate (nvp-abbrev-grab))
                      type))))))

(defun nvp-elisp-abbrev-expand-fn-p ()
  "Return t if function should expand."
  (nvp-elisp--abbrev-expand-p 'boundp))

(defun nvp-elisp-abbrev-expand-var-p ()
  "Return t if variable should expand."
  (nvp-elisp--abbrev-expand-p 'fboundp))


;; ------------------------------------------------------------
;;; Imenu

(let-when-compile
    ((prefix "^;;\\(?:;\\{1,2\\}\\|[*]\\{1,2\\}\\| |\\)\\s-+")
     (hdr-regex (concat prefix "\\([^#;].*\\)\\s-*$"))
     (pkg-hdrs
      (concat prefix (rx (group (or "Commentary:" "Code:" "Documentation:"
                                    "History:" "ChangeLog:" "Change Log:"
                                    (seq (* nonl) ".el" eow (* white)
                                         (or "ends here" "---"))))))))
  (let* ((var-re (eval-when-compile
                   (rx bol (* white) "("
                       (or "defvar-keymap"
                           (seq (regexp "nvp[:]")
                                (or "bindings" "defvar" "define")
                                (regexp "[^ \t\n]*")))
                       (+ white)
                       (group (+ (or (syntax word) (syntax symbol)
                                     (regexp "\\\\.")))))))
         (sub-hdr-re (eval-when-compile
                       (rx bol (* white) ";;" (+ (or "-" "*")) (* space)
                           (group (+ (not (or "-" "\n"))))
                           (* (or space "-" "*"))
                           eol)))
         (lib-re "^;;\\s-*[*]\\s-*\\(?:[Ll]ibs?\\):\\s-*\\([[:alnum:]- /]+\\)")
         (header-p
          (eval-when-compile
            (macroexpand-1
             ;; Don't include default package headers, beginning/end of file
             `(lambda ()
                (let (done)
                  (while (and (not (or done (bobp)))
                              (re-search-backward ,hdr-regex nil t))
                    (setq done (not (looking-at-p ,pkg-hdrs))))
                  done))))))

    (defalias 'nvp-elisp--imenu-header (byte-compile header-p))

    (defconst nvp-elisp-imenu-headers
      `( :headers ((nil nvp-elisp--imenu-header 1))
         :headers-1 (("Headers" nvp-elisp--imenu-header 1)
                     ("Libs" ,lib-re 1)
                     ("Variables" ,var-re 1))
         :headers-2 (("Sub-Headers" ,sub-hdr-re 1))))))

(provide 'nvp-elisp)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-elisp.el ends here
