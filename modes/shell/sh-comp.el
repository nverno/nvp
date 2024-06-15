;;; sh-comp.el --- Completion for sources in sh script -*- lexical-binding: t; -*-
;;; Commentary:

;; Caches completion candidates from sourced files.
;; Candidates are determined using imenu w/ `sh-comp-imenu-expression'.
;; Cache is recalculated for a file when its modification time changes.
;;
;; Completion at point:
;; - variables => sh-comp + environment variables since bash-completion
;;                doesn't account for variables defined in scripts
;; - local vars
;; - flags => bash-completion
;; - commands => bash-completion + sh-comp; bash-completion doesn't account
;;               for sourced functions
;;
;; Xref:
;; Backend jumps to sourced functions/variables + functions/globals defined
;; in current buffer (ignores local variables)

;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'xref)
(require 'sh-script)
(require 'company)
(require 'imenu)
(nvp:decls :v (bash-completion-process-timeout bash-completion-use-separate-processes)
           :f (bash-completion--get-process
               bash-completion--parse bash-completion--customize
               bash-completion--completion-table-with-cache bash-completion-comm
               bash-completion-reset bash-completion--stub-start))

(defvar sh-comp-use-bash-completion nil
  "Non-nil to use `bash-completion' during completion at point.")

;; This is useful for including local sources that are included via non-constant
;; expressions, e.g
;;    . "$DIR/../inc.sh"
;; where $DIR is not in the `process-environment', and wouldn't be expanded.
(defvar sh-comp-additional-sources ()
  "List of additional files to include. ")
;;;###autoload
(put 'sh-comp-additional-sources 'safe-local-variable 'listp)

(eval-when-compile
  (defsubst sh-comp--expand-file-names (stubs)
   (cl-loop for stub in stubs
      as file = (expand-file-name (substitute-in-file-name stub))
      when (file-exists-p file)
      collect file)))

(defun sh-comp-add-source (file &optional global)
  "Add FILE to `sh-comp-additional-sources'.
With prefix, add to GLOBAL sources."
  (interactive "f\nP")
  (unless global (make-local-variable 'sh-comp-additional-sources))
  (cl-pushnew file sh-comp-additional-sources :test #'string=))

(defun sh-comp-remove-source (file)
  "Remove FILE from `sh-comp-additional-sources'."
  (interactive
   (list (completing-read "Remove: " sh-comp-additional-sources nil t)))
  (setq sh-comp-additional-sources
        (--filter (not (string= it file)) sh-comp-additional-sources)))

(eval-when-compile
 ;; mapping for symbols to annotations
 (defvar sh-comp-symbol-annotation
   '((function " <f>") (local " <v>") (global " <V>") (envvar " <E>")))

 (defmacro sh-comp:annotation (symbol)
   `(eval-when-compile (cadr (assoc ',symbol sh-comp-symbol-annotation)))))

;; imenu regexp used to gather completion candidates
;; car of entry used to match type
(rx-let ((ident (group-n 1 (any "_" alpha) (* (any "_" alnum))))
         (^ (seq line-start (* space)))
         (sep (seq (+ space)))
         (sep* (seq (* space))))
  (let-when-compile (;; function FOO()? or FOO()
                     (func (rx ^ (or (seq "function" sep ident sep* (? (literal "()")))
                                     (seq ident sep* (literal "()")))))
                     ;; Global variable: [export|declare] [-x]* VAR=?
                     ;;              or: VAR=(VAL)?
                     (global-var (rx ^ (or (seq (or "export" "declare") sep
                                                (* (seq "-" alpha sep)) ; optional flags
                                                ident (? "="))
                                           (seq ident "=")))))
    (defvar sh-comp-imenu-expression
      (eval-when-compile
        `((,(sh-comp:annotation function) ,func 1)
          (,(sh-comp:annotation global) ,global-var 1))))))

;; track candidates from sourced files
(defvar sh-comp-db (make-hash-table :test 'equal))

(cl-defstruct (sh-comp-dbfile
               (:constructor sh-comp-make-dbfile)
               (:copier nil))
  functions
  variables
  sources
  modtime)

;; gather sourced files from buffer
(defun sh-comp--buffer-sources ()
  (let ((srcs sh-comp-additional-sources))
    (save-excursion
      (goto-char (point-min))
      ;; might not be called in a `sh-mode' buffer
      (with-syntax-table sh-mode-syntax-table
        (while (re-search-forward "\\_<\\(source\\|\\.\\)\\_>" nil 'move)
          (let ((syntax (syntax-ppss)))
            ;; ignore commented out / in strings
            (and (not (nth 3 syntax))
                 (not (nth 4 syntax))
                 (looking-at
                  (eval-when-compile
                    (concat
                     ;; quoted
                     "[ \t]*\\(?:\"\\(?1:[^\"]+\\)\\|"
                     ;; or unquoted
                     "\\(?1:[^\n\t ]+\\)\\)")))
                 ;; FIXME: add known globals to process env. ?
                 (push (match-string 1) srcs))))))
    (cl-remove-duplicates (sh-comp--expand-file-names srcs))))

;; use imenu to create candidate list from buffer
(defun sh-comp--buffer-candidates (fname)
  (ignore-errors 
    (with-syntax-table sh-mode-syntax-table
      (let* ((imenu-use-markers (buffer-file-name))
             (index (if imenu-auto-rescan (imenu--make-index-alist)
                      (cdr (imenu--make-index-alist)))))
        (when index
          (cl-loop for (type . vals) in index
             do
               (cl-loop for (elem . pos) in vals
                        do (add-text-properties
                            0 1 (list 'annot type
                                      'loc (cons fname (line-number-at-pos pos)))
                            elem)))
          index)))))

;; update sources/candidates for FILE in DBFILE entry
;; Note: make sure local vars get lexically bound in temp buffer
(defun sh-comp--file-update (file dbfile &optional recurse imenu-regexp)
  (or imenu-regexp (setq imenu-regexp sh-comp-imenu-expression))
  (let ((additional-sources sh-comp-additional-sources))
    (with-temp-buffer
      (insert-file-contents file)
      (let* ((imenu-generic-expression (or imenu-regexp sh-comp-imenu-expression))
             (imenu-create-index-function #'imenu-default-create-index-function)
             (sh-comp-additional-sources additional-sources)
             (srcs (sh-comp--buffer-sources))
             (cands (sh-comp--buffer-candidates file)))
        (setf (sh-comp-dbfile-functions dbfile)
              (cdr (assoc (sh-comp:annotation function) cands)))
        (setf (sh-comp-dbfile-variables dbfile)
              (cdr (assoc (sh-comp:annotation global) cands)))
        (setf (sh-comp-dbfile-sources dbfile) srcs)
        (puthash file dbfile sh-comp-db)
        (when recurse
          (dolist (src srcs)
            (when (not (equal src file))
              (sh-comp-file-candidates
               src nil recurse imenu-regexp 'no-return))))))))

(eval-when-compile
  (defsubst sh-comp--getter (type)
   (if (eq type 'all)
       (lambda (db) (append (sh-comp-dbfile-functions db)
                       (sh-comp-dbfile-variables db)))
     (intern (concat "sh-comp-dbfile-" (symbol-name type))))))

;; Get/cache candidates for FILE
;; Cache is created when empty or the file's modification time has changed
;; if RECURSE is non-nil, return candidates from all files sourced recursively
(defun sh-comp-file-candidates (file type &optional
                                         recurse imenu-regexp no-return)
  (let* ((attr (file-attributes file 'integer))
         (modtime (and attr (nth 5 attr)))
         (dbfile (or (gethash file sh-comp-db nil) (sh-comp-make-dbfile))))
    (when (not (equal modtime (sh-comp-dbfile-modtime dbfile)))
      (setf (sh-comp-dbfile-modtime dbfile) modtime)
      (sh-comp--file-update file dbfile recurse imenu-regexp))
    (unless no-return
      (let ((getter (sh-comp--getter type))
            res srcs)
        (if (null recurse) (funcall getter dbfile)
          (cl-labels ((build-res
                       (srcfile)
                       (when-let ((db (gethash srcfile sh-comp-db)))
                         (setq res (append res (funcall getter db)))
                         (dolist (s (sh-comp-dbfile-sources db))
                           (unless (member s srcs)
                             (push s srcs)
                             (build-res s))))))
            (build-res file))
          (delete-dups res))))))

;;;###autoload
(defun sh-comp-candidates (type &optional file)
  "List of completion targets from current buffer or FILE and all recursively
sourced files."
  (sh-comp-file-candidates
   (or file (buffer-file-name)) type 'recurse sh-comp-imenu-expression))

;; -------------------------------------------------------------------
;;; Completion

(nvp:decl require-process -parse -stub-start -customize
  -completion-table-with-cache comm reset :pre "bash-completion")

(defvar sh-comp-syntax-table
  (let ((tab sh-mode-syntax-table))
    (modify-syntax-entry ?$ "'" tab)
    tab))

;;; Locals: lexical + globals

;; Like `narrow-to-defun', but only narrow if point is actually inside a function.
;; Retrun point at start of function if narrowing was done.
(defun sh-comp--narrow-lexically ()
  (save-excursion
    (widen)
    (when-let* ((ppss (syntax-ppss))
                (parens (nth 9 ppss))  ;start of outermost parens
                (start (point)))
      (goto-char (car parens))
      (and (eq (char-after) ?{)
           (progn
             (narrow-to-region
              (point)
              (condition-case nil      ;unterminated sexps
                  (progn (forward-sexp) (point))
                (scan-error start)))
             (car parens))))))

;; variables before point in current function:
;; local ...
;; or anything like: '<varname>='
;; so it may get non-locals as well.
(defun sh-comp--local-variables (&optional pos)
  (when pos (goto-char pos))
  (save-excursion
    (with-syntax-table sh-mode-syntax-table
      (let* ((func-start (sh-comp--narrow-lexically))
             vars)
        (widen)
        (when func-start
          ;; collect local variables
          (while (re-search-backward
                  "^[ \t]*local[ \t]+\\([^=\n]+\\)\\(=\\)?\\|\\(\\_<[[:alnum:]_]+\\)="
                  func-start t)
            ;; skip strings/comments
            (unless (nth 8 (syntax-ppss))
              (let ((pos (or (match-beginning 3) (match-beginning 2) (match-beginning 1))))
               (-if-let (var (or (match-string 3) (match-string 2)))
                   (progn
                     (add-text-properties
                      0 1 `(annot ,(sh-comp:annotation local) loc ,pos) var)
                     (push var vars))
                 (dolist (var (split-string (match-string 1) nil 'omit "[ \t]"))
                   (add-text-properties
                    0 1 `(annot ,(sh-comp:annotation local) loc ,pos) var)
                   (push var vars))))))
          (delete-dups vars))))))

;; only update once while writing a symbol
;; see #<marker at 14695 in elisp-mode.el.gz>
(defvar sh-comp--local-variables-completion-table
  (let (lastpos lastvars)
    (letrec ((hookfun (lambda ()
                        (setq lastpos nil)
                        (remove-hook 'post-command-hook hookfun))))
      (completion-table-dynamic
       (lambda (_string)
         (save-excursion
           (skip-syntax-backward "_w")
           (let ((newpos (cons (point) (current-buffer))))
             (unless (equal lastpos newpos)
               (add-hook 'post-command-hook hookfun)
               (setq lastpos newpos)
               (setq lastvars (sh-comp--local-variables)))))
         lastvars)))))

(defsubst sh-comp-variable-p (pos)
  (or (eq (char-before pos) ?$)
      (and (eq (char-before pos) ?{)
           (eq (char-before (1- pos)) ?$))))

;;;###autoload
(defun sh-comp-completion-at-point ()
  ;; XXX: in comments complete for docs only
  (with-syntax-table sh-comp-syntax-table
    (nvp:unless-ppss 'cmt
      (let* ((bash-completion-process-timeout 0.5)
             (bash-completion-use-separate-processes t)
             (pos (point))
             (beg (condition-case nil
                      (save-excursion
                        (backward-sexp 1)
                        (skip-syntax-forward "'")
                        (point))
                    (scan-error pos)))
             (var (sh-comp-variable-p beg))
             (flag (eq (char-after beg) ?-))
             (use-comp (and sh-comp-use-bash-completion
                            (null var)))
             (proc (and use-comp
                        (bash-completion--get-process)))
             (comp (when use-comp
                     (bash-completion--parse
                      (or (save-excursion (sh-beginning-of-command)) beg)
                      pos
                      (process-get proc 'wordbreaks)
                      ;; (process-get proc 'bash-major-version)
                      )))
             (end (cond
                   ;; complete after '$'
                   ((and var (memq (char-syntax (char-after beg)) '(?> ?\))))
                    beg)
                   ;; use bash completion after '--'
                   ;; XXX: any way to get completion for '-' ???
                   (flag
                    (when use-comp
                      (setq beg (bash-completion--stub-start comp)))
                    pos)
                   (t
                    (unless (or (eq beg (point-max))
                                (member (char-syntax (char-after beg))
                                        '(?\s ?\" ?\( ?\))))
                      (condition-case nil
                          (save-excursion
                            (goto-char beg)
                            (forward-sexp 1)
                            (when (>= (point) pos)
                              (point)))
                        (scan-error pos)))))))
        ;; (when use-comp
        ;;   (bash-completion--customize comp proc))
        (when (and beg end)
          (nconc (list beg end)
                 (cond
                  ((and use-comp flag)
                   (list (bash-completion--completion-table-with-cache
                          comp proc)))
                  (var
                   (list (completion-table-merge
                          sh-comp--local-variables-completion-table
                          (completion-table-dynamic
                           (lambda (_string) (completion--make-envvar-table)))
                          (completion-table-dynamic
                           (lambda (_string) (sh-comp-candidates 'variables))))
                         :annotation-function
                         (lambda (s) (or (get-text-property 0 'annot s) " <E>"))
                         :company-location
                         (lambda (s) (--when-let (get-text-property 0 'loc s)
                                  (if (consp it) it
                                    (cons (buffer-file-name) (line-number-at-pos it)))))))
                  (t
                   (list (completion-table-merge
                          (when use-comp
                           (condition-case nil
                               (bash-completion--completion-table-with-cache
                                comp proc)
                             (error (prog1 nil (bash-completion-reset)))))
                          (completion-table-dynamic
                           (lambda (_string) (sh-comp-candidates 'functions))))
                         :annotation-function
                         (lambda (s) (or (get-text-property 0 'annot s) " <S>"))
                         :company-location
                         (lambda (s) (--when-let (get-text-property 0 'loc s)
                                  (if (consp it) it
                                    (cons (buffer-file-name) (line-number-at-pos it))))))))
                 (list :exclusive 'no)))))))

;;; Company

;;;###autoload
(defun company-sh-comp (command &optional arg &rest _args)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-sh-comp))
    (prefix (nvp:unless-ppss 'cmt
              (and (derived-mode-p 'sh-mode)
                   (company-grab-symbol))))
    (annotation (get-text-property 0 'annot arg))
    (location (-if-let (marker (get-text-property 0 'marker arg))
                  (and (markerp marker)
                       (cons (marker-buffer marker) (marker-position marker)))))
    (candidates
     (if (sh-comp-variable-p (- (point) (length arg)))
         (all-completions arg (sh-comp-candidates 'variables))
       (all-completions arg (sh-comp-candidates 'functions))))
    (require-match 'never)
    (duplicates nil)))

;; -------------------------------------------------------------------
;;; Xref

(nvp:decl xref-make xref-item-location)

;;;###autoload
(defun sh-comp--xref-backend () 'sh-comp)

;;;###autoload
(defun nvp-sh-find-definitions ()
  "Find definitions using `sh-comp' backend explicitly."
  (interactive)
  (let* ((xref-backend-functions '(sh-comp--xref-backend t))
         (thing (xref-backend-identifier-at-point 'sh-comp)))
    (xref-find-definitions thing)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql 'sh-comp)))
  (completion-table-dynamic
   (lambda (_string) (sh-comp-candidates 'all (buffer-file-name))) 'switch))

;;; TODO: if at a source => detect variable in path, jump to path in source line
(cl-defmethod xref-backend-identifier-at-point ((_backend (eql 'sh-comp)))
  (save-excursion
    (skip-chars-forward "[:alnum:]_")
    (let ((end (point))
          (beg (progn (skip-chars-backward "[:alnum:]_")
                      (point))))
      (when (> end beg)
        (buffer-substring beg end)))))

(defun sh-comp--make-xref-location (ident file)
  (-if-let (pos (let ((txt (assoc-string ident (sh-comp--local-variables))))
                  (and txt (get-text-property 0 'loc txt))))
      ;; local variable
      (xref-make-sh-location file pos)
    (let ((db (gethash file sh-comp-db)))
      (unless db
        (sh-comp-file-candidates file 'all 'recurse nil t))
      (if (and (file-exists-p ident)
               (cl-member (expand-file-name (substitute-in-file-name ident))
                          (sh-comp-dbfile-sources db) :test #'equal))
          (xref-make-sh-location ident 0)
        (cl-labels ((getter
                      (f)
                      (let ((db (gethash f sh-comp-db)))
                        (-when-let (id (or (cl-find ident (sh-comp-dbfile-functions db)
                                                    :test #'string= :key #'car)
                                           (cl-find ident (sh-comp-dbfile-variables db)
                                                    :test #'string= :key #'car)))
                          (xref-make-sh-location f (cdr id))))))
          (-if-let (xref (getter file)) xref
            (let* ((srcs (sh-comp-candidates 'sources file)) done xref)
              (while (and srcs (not done))
                (when (setq xref (getter (pop srcs)))
                  (setq done t)))
              xref)))))))

(cl-defmethod xref-backend-definitions ((_backend (eql 'sh-comp)) identifier)
  (-when-let (loc (sh-comp--make-xref-location identifier (buffer-file-name)))
    (list (xref-make identifier loc))))

(cl-defstruct (xref-sh-location
               (:constructor xref-make-sh-location (file pos)))
  "Location of sh symbol definition."
  file pos)

(cl-defmethod xref-location-marker ((l xref-sh-location))
  (pcase-let (((cl-struct xref-sh-location file pos) l))
    (let ((buffer (find-file-noselect file)))
      (with-current-buffer buffer
        (save-excursion
          (goto-char pos)
          (point-marker))))))

(provide 'sh-comp)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; sh-comp.el ends here
