;;; nvp-make-completion.el --- makefile completion -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Completion, Eldoc, and Xref backends for makefiles
;;
;; * Builtin function completion/docs from make-doc info files.
;; * Dynamic completion for variables/rules come from make's internal database:
;; make -prns -f %s .DEFAULT_GOAL=
;; -p [--print-data-base] -s [--silent] -n [--just-print]
;; -q [--question]
;; -r [--no-builtin-rules]
;; -R [--no-builtin-variables]
;; > diff <(make -pqrns) <(make -pqrRns)
;;
;; Variable candidates have form:
;; (<varname> [value] <type> [file] [lineno])
;; - value, file, lineno may be nil
;; - type is one of: [environment, makefile, default, automatic, override]
;;
;; Xref
;; - makefile variables, rules, and defines have filenames + locations
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:req 'nvp-makefile 'subrs)
(require 'xref)
(require 'nvp)
(nvp:auto "info-look" 'info-lookup->completions)
(nvp:auto "s" 's-lowercase-p)
(nvp:decls :f (xref-make xref-location) :v (info-lookup-other-window-flag))

(defvar nvp-makefile--dir (file-name-directory (nvp:load-file-name)))

(defconst nvp-makecomp-program
  (expand-file-name "bin/makevars.awk" nvp-makefile--dir))

(defvar nvp-makecomp-program-switches "-prns")

(eval-when-compile
  (defvar nvp-makecomp--annotation
    '((variable " <v>") (rule " <r>") (function " <f>") (environment " <e>")))

  (defmacro nvp-makecomp:annotation (symbol)
    `(eval-when-compile (cadr (assoc ',symbol nvp-makecomp--annotation))))
  
  (defsubst nvp-makecomp--getter (type)
    (if (eq type 'all)
        (lambda (db) (append (nvp-makecomp-file-variables db)
                        (nvp-makecomp-file-rules db)))
      (intern (concat "nvp-makecomp-file-" (symbol-name type))))))

(cl-defstruct (nvp-makecomp-file
               (:constructor nvp-makecomp-make-file)
               (:copier nil))
  variables                             ;variables from included files as well
  rules
  modtime)

(defvar nvp-makecomp-db (make-hash-table :test #'equal))

(defun nvp-makecomp--update (file dbfile)
  (-when-let (data (car (read-from-string
                         (shell-command-to-string
                          ;; when no targets in file there is output to stderr
                          (format "make %s -f %s .DEFAULT_GOAL= 2>/dev/null | %s"
                                  nvp-makecomp-program-switches
                                  file nvp-makecomp-program)))))
    (setf (nvp-makecomp-file-variables dbfile) (alist-get 'variables data))
    (setf (nvp-makecomp-file-rules dbfile) (alist-get 'rules data))))

;;;###autoload
(defun nvp-makecomp-candidates (type &optional file)
  "List of completion targets of TYPE.
TYPE is one of \\='all, \\='variables or \\='rules."
  (or file (setq file (buffer-file-name)))
  (let* ((attr (file-attributes file 'integer))
         (modtime (and attr (nth 5 attr)))
         (dbfile (or (gethash file nvp-makecomp-db nil)
                     (nvp-makecomp-make-file))))
    (when (not (equal modtime (nvp-makecomp-file-modtime dbfile)))
      (setf (nvp-makecomp-file-modtime dbfile) modtime)
      (nvp-makecomp--update file dbfile)
      (puthash file dbfile nvp-makecomp-db))
    (funcall (nvp-makecomp--getter type) dbfile)))

;; -------------------------------------------------------------------
;;; Completion

(defvar nvp-makecomp-info-completion-table
  (let ((syms (info-lookup->completions 'symbol 'makefile-mode)))
    (cl-loop for (sym . _) in syms
       when (s-lowercase-p sym)
       ;; assume lowercase symbols are functions
       do (put-text-property
           0 1 'annotation (nvp-makecomp:annotation function) sym)
       collect sym)))

(defun nvp-makecomp--location (type str)
  (-when-let (data (assoc str (nvp-makecomp-candidates type)))
    (pcase type
      ('variables
       (and (nth 3 data) (cons (nth 3 data) (nth 4 data))))
      ('rules
       (and (nth 1 data) (cons (nth 1 data) (1- (nth 2 data))))))))

(defun nvp-makecomp--doc-buffer (str)
  (save-window-excursion
    (let ((display-buffer-overriding-action '(nil . ((inhibit-switch-frame . t))))
          (info-lookup-other-window-flag nil))
      (ignore-errors
        (info-lookup-symbol str 'makefile-mode)
        (current-buffer)))))

;;;###autoload
(defun nvp-makecomp-completion-at-point ()
  (-when-let* (((beg . end) (bounds-of-thing-at-point 'symbol)))
    (let
        ((table
          (cond
           ;; $(... or ${...
           ((nvp:makefile-variable-or-function-p beg)
            (list
             (completion-table-merge
              (completion-table-with-cache
               (lambda (_string) (nvp-makecomp-candidates 'variables)))
              nvp-makecomp-info-completion-table)
             :annotation-function (lambda (s) (or (get-text-property 0 'annotation s)
                                             (nvp-makecomp:annotation variable)))
             :company-location (apply-partially #'nvp-makecomp--location 'variables)
             :company-doc-buffer #'nvp-makecomp--doc-buffer))
           ;; $(call <varname>
           ((save-excursion
              (goto-char beg)
              (looking-back "(call\\s-+" (line-beginning-position)))
            (list
             (completion-table-with-cache
              (lambda (_string) (nvp-makecomp-candidates 'variables)))
             :annotation-function (lambda (s) (or (get-text-property 0 'annotation s)
                                             (nvp-makecomp:annotation variable)))
             :company-location (apply-partially #'nvp-makecomp--location 'variables)
             :company-doc-buffer #'nvp-makecomp--doc-buffer))
           ;; rules => after <rulename>: ...
           ((nvp:makefile-rule-line-p)
            (list
             (completion-table-with-cache
              (lambda (_string) (nvp-makecomp-candidates 'rules)))
             :annotation-function (lambda (s) (or (get-text-property 0 'annotation s)
                                             (nvp-makecomp:annotation rule)))
             :company-location (apply-partially #'nvp-makecomp--location 'rules)))
           (t nil))))
      (when table
        (nconc (list beg end)
               table
               (list :exclusive 'no))))))

;;; Eldoc

(defun nvp-makecomp-current-thing ()
  (let ((bnds (bounds-of-thing-at-point 'symbol)))
    (if (and bnds (eq ?$ (char-before (car bnds))))
        (list 'autovar (1- (car bnds)) (cdr bnds))
      (let ((lim (nvp:point 'boll)))
        (save-excursion
          (or (progn
                (ignore-errors (up-list -1 t t))
                (when (and (not (eobp))
                           (>= (point) lim))
                  (forward-char 1)
                  (and (setq bnds (bounds-of-thing-at-point 'symbol))
                       (nvp:makefile-variable-or-function-p (car bnds))
                       (list 'fun-or-var (car bnds) (cdr bnds)))))
              (progn
                (goto-char lim)
                (and (looking-at "^[ ]*\\([^#:\n\t ]+\\):")
                     (list 'target (match-beginning 1) (match-end 1))))))))))

(defvar nvp-makecomp--eldoc-cache (make-hash-table :test #'equal))

(defun nvp-makecomp-info-sig (thing &optional type)
  (or (gethash thing nvp-makecomp--eldoc-cache)
      (save-window-excursion
        (let ((display-buffer-overriding-action '(nil . ((inhibit-switch-frame . t))))
              (info-lookup-other-window-flag nil))
          (ignore-errors (info-lookup-symbol thing))
          (save-excursion
            (--> (pcase type
                   ('autovar
                    (forward-line 1)
                    (skip-syntax-forward " ")
                    (buffer-substring-no-properties
                     (point) (cdr (bounds-of-thing-at-point 'sentence))))
                   ('target
                    (and-let* ((target (thing-at-point 'sentence t)))
                      (replace-regexp-in-string "[\n\r\t ]+" " " target)))
                   (_
                    (goto-char (point-min))
                    (when (re-search-forward (concat "\\([$](" thing ".*)\\)") nil t)
                      (match-string 1))))
                 (puthash thing it nvp-makecomp--eldoc-cache)))))))

;;;###autoload
(defun nvp-makecomp-eldoc-function (callback &rest _ignored)
  (when-let* ((thing (nvp-makecomp-current-thing))
              (type (car thing))
              (face (if (eq 'autovar type) 'font-lock-variable-name-face
                      'font-lock-function-name-face))
              (sym (buffer-substring-no-properties (cadr thing) (caddr thing)))
              (sig (or (and (eq 'fun-or-var type)
                            (-some--> (assoc sym (nvp-makecomp-candidates 'variables))
                              (prog1 (nth 1 it)
                                (setq face 'font-lock-variable-name-face
                                      type 'variable))))
                       (nvp-makecomp-info-sig sym type))))
    (when (length> sig 80)
      (setq sig (concat (substring sig nil 75) "..."))
      (and (not (eq 'variable type))
           (puthash sym sig nvp-makecomp--eldoc-cache)))
    (funcall callback sig :thing sym :face face)))


;; -------------------------------------------------------------------
;;; Xref

;;;###autoload
(defun nvp-makecomp--xref-backend () 'makefile)

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql makefile)))
  (completion-table-dynamic
   (lambda (_string) (nvp-makecomp-candidates 'all)) 'do-switch-buffer))

(defun nvp-makecomp--make-xref (ident)
  (-if-let (data (assoc ident (nvp-makecomp-candidates 'variables)))
      (and (nth 3 data)
           (xref-make-file-location (nth 3 data) (nth 4 data) 0))
    (-if-let (data (assoc ident (nvp-makecomp-candidates 'rules)))
        (and (nth 1 data)
             (xref-make-file-location (nth 1 data) (1- (nth 2 data)) 0)))))

(cl-defmethod xref-backend-definitions ((_backend (eql makefile)) ident)
  (-when-let (loc (nvp-makecomp--make-xref ident))
    (list (xref-make ident loc))))

(provide 'nvp-make-completion)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-make-completion.el ends here
