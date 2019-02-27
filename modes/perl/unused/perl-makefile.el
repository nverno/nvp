;;* libs: eproject
(require 'eproject)

;; local files
(eval-when-compile
  (require 'cperl-macros))
(require 'perl-utils)
(require 'perl-project)

;; perl-module-lib-file-p,
;; perl-module-test-file-p

;; -------------------------------------------------------------------
;;; Util

;; Executes MOVE-AROUND and returns `(point)' there
(defmacro perl-makefile-bounds-of (&rest move-around)
  `(save-excursion ,@move-around (point)))

(defsubst perl-makefile--sort-modules (a b)
  (string< (if (listp a) (car a) a)
           (if (listp b) (car b) b)))

;; -------------------------------------------------------------------
;;; Parse requires

;; Returns t if we are looking-at requires or build_requires.
;; Sets match; capture 1 is the requires keyword (or
;; build_requires), capture 2 is the module name
(defsubst perl-makefile--looking-at-requires ()
  (looking-at
   "\\(\\(?:build_\\)?requires\\)[[:space:]]+[^A-Za-z9-0_]+\\([A-Za-z0-9_:]+\\)"))

;; Looks for a requires 'Foo' or build_requires 'Foo' statement
;; on the current line of the current buffer and updates the requires
;; and build-requires lists accordingly
(defun perl-makefile--parse-requires-line ()
  (when (perl-makefile--looking-at-requires)
    (let* ((type (match-string-no-properties 1))
           (module (match-string-no-properties 2))
           (version (save-excursion ; parse "Foo::Bar" => 'foo' into 'foo'
                      (let ((eol (save-excursion (end-of-line) (point))))
                        (goto-char (match-end 0))
                        (forward-char)
                        (skip-chars-forward "[:space:]=>")
                        (if (re-search-forward
                             "\\(['\"]\\)\\(.+\\)\\1" eol t)
                            (match-string-no-properties 0)
                          nil))))
           (result (if version (cons module version) module))
           slot)
      (cond
       ((equal type "requires") (setq slot 'requires))
       ((equal type "build_requires") (setq slot 'build-requires)))
      (add-to-list slot result))))

;; Determines if the current buffer is a Module::Install
;; Makefile.PL.  Throws an error if not, otherwise returns t.
(defsubst perl-makefile--module-install-p ()
  (save-excursion-rewind ; check for MI
   (if (not (search-forward "use inc::Module::Install" nil t))
       (error "Not a Module::Install Makefile.PL!")
     t)))

;; Parse a Makefile.PL in the current buffer and return a list of
;; prereqs in a (requires . build-requires) cons cell
(defun perl-makefile--parse ()
  (save-excursion
    (goto-char (point-min))
    (perl-makefile--module-install-p)
    (let (requires build-requires)
      (while (zerop (forward-line 1))
        (perl-makefile--parse-requires-line))
      (cons requires build-requires))))

;; -------------------------------------------------------------------
;;; Add requires

(defsubst perl-makefile--write-requires-line (type def)
  (if (not (listp def))
      (insert (format "%s '%s';\n" type def))
    (insert (format "%s '%s' => %s;\n" type (car def) (cdr def)))))

;; Given a (requires . build-require) cons cell REQUIRES, kill
;; the existing requires and build_requires statements and
;; regenerate them from the REQUIRES list
(defun perl-makefile--rewrite-requires (requires)
  (let ((r (car requires))
        (b (cdr requires)))
    (setq r (sort r 'perl-makefile--sort-modules))
    (setq b (sort b 'perl-makefile--sort-modules))
    (setq requires (cons r b)))
  (let (where)
    (save-excursion-rewind ; first, blow away requires
      (while (zerop (forward-line 1))
        (if (perl-makefile--looking-at-requires)
            (progn
              (if (not where) (setq where (point))) ; save start position
              (delete-region (perl-makefile-bounds-of (beginning-of-line))
                             (perl-makefile-bounds-of (end-of-line) (forward-char)))
              (backward-char)))))
      (save-excursion
        ; goto where we want to insert
        (if where (goto-char where) ; where the old stuff was
          (perl-makefile-bounds-of                ; or before WriteAll()
           (or (re-search-forward "WriteAll" nil t) (goto-char (point-max)))))
        (if (not (save-excursion (forward-line -1) (looking-at "^$")))
            (insert "\n"))
        (mapc (lambda (arg) (perl-makefile--write-requires-line (car arg) (cdr arg)))
              (append
               (mapcar (lambda (arg) (cons "requires" arg))
                       (car requires))
               (mapcar (lambda (arg) (cons "build_requires" arg))
                       (cdr requires)))))))

;; Visits MAKEFILE and adds elements of REQUIRES to the requires
;; section of it; if BUILD-REQUIRES is non-nil, add the elements of
;; the list to the build_requires section.
(defun perl-makefile--add-requires (makefile requires
                                             &optional build-requires)
  (save-excursion
    (let ((kill-when-done (not (find-buffer-visiting makefile)))
          (cperl-no-flymake t)) ; suppress flymake for a file we won't even see
      (protect-unwind (if kill-when-done (kill-buffer nil))
                      (find-file makefile)
                      (let* ((all (perl-makefile--parse))
                             (r (car all))
                             (b (cdr all)))
                        (mapc (lambda (arg) (add-to-list 'r arg)) requires)
                        (mapc (lambda (arg) (add-to-list 'b arg)) build-requires)
                        (perl-makefile--rewrite-requires (cons r b))
                        (save-buffer))))))

(defun perl-makefile--add-by-file-type (&rest modules)
  (let* ((f (buffer-file-name))
         (makefile (perl-project-look-for-makefile)))
    (cond
     ((perl-module-lib-file-p f)
      (perl-makefile--add-requires makefile modules))
     ((perl-module-test-file-p f)
      (perl-makefile--add-requires makefile nil modules))
     (t (error "Not a library or test file!")))))

;; -------------------------------------------------------------------
;;; Interface

(defun perl-makefile-add-requires ()
  "Add requires to Makfile.PL."
  (interactive)
  (let ((module (read-with-default "Module" (thing-at-point 'perl-module)
                                   "You must specify a module!")))
    (perl-makefile--add-by-file-type module)))

(defun perl-makefile-jump-to-makefile ()
  (interactive)
  (find-file (perl-project-look-for-makefile)))

(provide 'perl-makefilefile)
