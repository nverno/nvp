;;; bash-source.el --- Completion for sources in sh script -*- lexical-binding: t; -*-
;;; Commentary:

;; Caches completion candidates from sourced files.
;; Candidates are determined using imenu w/ `bash-source-imenu-expression'.
;; Cache is recalculated for a file when its modification time changes.
;;
;; Completion at point:
;; - variables => bash-source + environment variables since bash-completion
;;                doesn't account for variables defined in scripts
;; - flags => bash-completion
;; - commands => bash-completion + bash-source; bash-completion doesn't account
;;               for sourced functions

;;; Code:
(eval-when-compile
  (require 'cl-lib))
(require 'company)
(require 'imenu)

(defvar bash-source-use-bash-completion t
  "Non-nil to use `bash-completion' during completion at point.")

(eval-when-compile
 ;; mapping for symbols to annotations
 (defvar bash-source-symbol-annotation
   '((function " <f>") (local " <v>") (global " <V>") (envvar " <E>")))

 (defmacro bash-source:annotation (symbol)
   `(eval-when-compile (cadr (assoc ',symbol bash-source-symbol-annotation)))))

;; imenu regexp used to gather completion candidates
;; car of entry used to match type
(defvar bash-source-imenu-expression
  `((,(bash-source:annotation function)
     ;; function FOO
     ;; function FOO()
     "^\\s-*function\\s-+\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*\\(?:()\\)?" 1)
    (,(bash-source:annotation function)
     ;; FOO()
     "^\\s-*\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*()" 1)
    ;; Global variables
    (,(bash-source:annotation global)
     "^\\([[:alpha:]_][[:alnum:]_]*\\)=" 1)))

;; track candidates from sourced files
(defvar bash-source-db (make-hash-table :test 'equal))

(cl-defstruct (bash-source-dbfile
               (:constructor bash-source-make-dbfile)
               (:copier nil))
  functions
  variables
  sources
  modtime)

;; gather sourced files from buffer
(defun bash-source--buffer-sources ()
  (save-excursion
    (goto-char (point-min))
    ;; might not be called in a `sh-mode' buffer
    (with-syntax-table sh-mode-syntax-table
      (let (srcs)
        (while (re-search-forward "\\_<\\(source\\|\\.\\)\\_>" nil 'move)
          (let ((syntax (syntax-ppss)) file)
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
                 (setq file (expand-file-name
                             (substitute-in-file-name
                              (match-string 1))))
                 (when (file-exists-p file)
                   (push file srcs)))))
        srcs))))

;; use imenu to create candidate list from buffer
(defun bash-source--buffer-candidates ()
  (ignore-errors 
    (with-syntax-table sh-mode-syntax-table
      (let* ((imenu-use-markers (buffer-file-name))
             (index (cdr (imenu--make-index-alist))))
        (when index
          (cl-loop for (type . vals) in index
             do
               (cl-loop for (elem . pos) in vals
                  do (add-text-properties 0 1 (list 'annot type) elem)))
          index)))))

;; update sources/candidates for FILE in DBFILE entry
(defun bash-source--file-update (file dbfile &optional recurse imenu-regexp)
  (or imenu-regexp (setq imenu-regexp bash-source-imenu-expression))
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((imenu-generic-expression (or imenu-regexp bash-source-imenu-expression))
           (imenu-create-index-function #'imenu-default-create-index-function)
           (srcs (bash-source--buffer-sources))
           (cands (bash-source--buffer-candidates)))
      (setf (bash-source-dbfile-functions dbfile)
            (cdr (assoc (bash-source:annotation function) cands)))
      (setf (bash-source-dbfile-variables dbfile)
            (cdr (assoc (bash-source:annotation global) cands)))
      (setf (bash-source-dbfile-sources dbfile) srcs)
      (puthash file dbfile bash-source-db)
      (when recurse
        (dolist (src srcs)
          (when (not (equal src file))
            (bash-source-file-candidates
             src nil recurse imenu-regexp 'no-return)))))))

(defsubst bash-source--getter (type)
  (if (eq type 'all)
      (lambda (db) (append (bash-source-dbfile-functions db)
                      (bash-source-dbfile-variables db)))
    (intern (concat "bash-source-dbfile-" (symbol-name type)))))

;; Get/cache candidates for FILE
;; Cache is created when empty or the file's modification time has changed
;; if RECURSE is non-nil, return candidates from all files sourced recursively
(defun bash-source-file-candidates (file type &optional
                                         recurse imenu-regexp no-return)
  (let* ((attr (file-attributes file 'integer))
         (modtime (and attr (nth 5 attr)))
         (dbfile (or (gethash file bash-source-db nil) (bash-source-make-dbfile))))
    (when (not (equal modtime (bash-source-dbfile-modtime dbfile)))
      (setf (bash-source-dbfile-modtime dbfile) modtime)
      (bash-source--file-update file dbfile recurse imenu-regexp))
    (unless no-return
      (let ((getter (bash-source--getter type))
            res srcs)
        (if (null recurse) (funcall getter dbfile)
          (cl-labels ((build-res
                       (srcfile)
                       (let ((db (gethash srcfile bash-source-db)))
                         (setq res (append res (funcall getter db)))
                         (dolist (s (bash-source-dbfile-sources db))
                           (unless (member s srcs)
                             (push s srcs)
                             (build-res s))))))
            (build-res file))
          (delete-dups res))))))

;;;###autoload
(defun bash-source-candidates (type &optional file)
  "List of completion targets from current buffer or FILE and all recursively \
sourced files."
  (bash-source-file-candidates
   (or file (buffer-file-name)) type 'recurse bash-source-imenu-expression))

;; -------------------------------------------------------------------
;;; Completion

(defvar bash-source-syntax-table
  (let ((tab sh-mode-syntax-table))
    (modify-syntax-entry ?$ "'" tab)
    tab))

(defsubst bash-source-variable-p (pos)
  (or (eq (char-before pos) ?$)
      (and (eq (char-before pos) ?{)
           (eq (char-before (1- pos)) ?$))))

(defun bash-source-completion-at-point ()
  (with-syntax-table bash-source-syntax-table
    (let* ((pos (point))
           (beg (condition-case nil
                    (save-excursion
                      (backward-sexp 1)
                      (skip-syntax-forward "'")
                      (point))
                  (scan-error pos)))
           (var (bash-source-variable-p beg))
           (flag (eq (char-after beg) ?-))
           (use-comp (and bash-source-use-bash-completion
                          (null var)))
           (proc (and use-comp (bash-completion-require-process)))
           (comp (when use-comp
                   (bash-completion--parse
                    (or (save-excursion (sh-beginning-of-command)) beg)
                    pos
                    (process-get proc 'wordbreaks)
                    (process-get proc 'bash-major-version))))
           (end (cond
                 ;; complete after '$'
                 ((and var (memq (char-syntax (char-after beg)) '(?> ?\))))
                  beg)
                 ;; use bash completion after '--'
                 (flag
                  (setq beg (bash-completion--stub-start comp))
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
      (when use-comp
        (bash-completion--customize comp proc))
      (when (and beg end)
        (nconc (list beg end)
               (cond
                ((and use-comp flag)
                 (list (bash-completion--completion-table-with-cache
                        (lambda (_) (bash-completion-comm comp proc)))))
                (var
                 (list (completion-table-merge
                        (completion-table-dynamic
                         (lambda (_string) (completion--make-envvar-table)))
                        (completion-table-dynamic
                         (lambda (_string) (bash-source-candidates 'variables))))
                       :annotation-function
                       (lambda (s) (or (get-text-property 0 'annot s) " <E>"))))
                (t
                 (list (completion-table-merge
                        (bash-completion--completion-table-with-cache
                         (lambda (_) (bash-completion-comm comp proc)))
                        (completion-table-dynamic
                         (lambda (_string) (bash-source-candidates 'functions))))
                       :annotation-function
                       (lambda (s) (or (get-text-property 0 'annot s) " <S>")))))
               (list :exclusive 'no))))))

;;; Company

;;;###autoload
(defun company-bash-source (command &optional arg &rest _args)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-bash-source))
    (prefix (nvp-unless-ppss 'cmt
              (and (derived-mode-p 'sh-mode)
                   (company-grab-symbol))))
    (annotation (get-text-property 0 'annot arg))
    (location (-if-let (marker (get-text-property 0 'marker arg))
                  (and (markerp marker)
                       (cons (marker-buffer marker) (marker-position marker)))))
    (candidates
     (if (bash-source-variable-p (- (point) (length arg)))
         (all-completions arg (bash-source-candidates 'variables))
       (all-completions arg (bash-source-candidates 'functions))))
    (require-match 'never)
    (duplicates nil)))

;; -------------------------------------------------------------------
;;; Xref

(defun bash-source--xref-backend () 'bash-source)

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql bash-source)))
  (completion-table-dynamic
   (lambda (_string) (bash-source-candidates 'all (buffer-file-name))) 'switch))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql bash-source)))
  (thing-at-point 'symbol))

(defun bash-source--make-xref-location (ident file)
  (cl-labels ((getter
               (f)
               (let ((db (gethash f bash-source-db)))
                 (-when-let (id (or (cl-find ident (bash-source-dbfile-functions db)
                                             :test #'string= :key #'car)
                                    (cl-find ident (bash-source-dbfile-variables db)
                                             :test #'string= :key #'car)))
                   (xref-make-bash-location f (cdr id))))))
    (-if-let (xref (getter file)) xref
      (let* ((srcs (bash-source-candidates 'sources file)))
        (cl-find-if #'getter srcs)))))

(cl-defmethod xref-backend-definitions ((_backend (eql bash-source)) identifier)
  (list
   (xref-make
    identifier
    ;; (eval-when-compile
    ;;   (macroexpand-all
    ;;    `(pcase (get-text-property 0 'annot identifier)
    ;;       (,(bash-source:annotation global)
    ;;        (put-text-property 0 1 'face 'font-lock-variable-name-face identifier))
    ;;       (,(bash-source:annotation local)
    ;;        (put-text-property 0 1 'face 'font-lock-variable-name-face identifier))
    ;;       (,(bash-source:annotation function)
    ;;        (put-text-property 0 1 'face 'font-lock-function-name-face identifier))
    ;;       (_ identifier))))
    (bash-source--make-xref-location identifier (buffer-file-name)))))

(defclass xref-bash-location (xref-location)
  ((pos :type fixnum :initarg :pos)
   (file :type string :initarg :file
         :reader xref-location-group))
  :documentation "Location of bash-source tag.")

(defun xref-make-bash-location (file pos)
  (make-instance 'xref-bash-location :file file :pos pos))

(cl-defmethod xref-location-marker ((l xref-bash-location))
  (with-slots (file pos) l
    (let ((buffer (find-file-noselect file)))
      (with-current-buffer buffer
        (save-excursion
          (goto-char pos)
          (point-marker))))))

;; (cl-defstruct (xref-bash-location
;;                (:constructor xref-make-bash-location (symbol type))
;;                (:copier nil)))

(provide 'bash-source)
;;; bash-source.el ends here
