;;; nvp-setup.el --- setup hooks/helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Mode setup
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'yasnippet)
(nvp:decls :v (smie-closer-alist smartparens-mode-map info-lookup-mode))

(cl-defstruct (nvp-mode-vars (:constructor nvp-mode-vars-make)
                             (:copier nil))
  "Store the local variables setup when a mode hook first runs."
  dir snippets abbr-file abbr-table docsets
  ;; functions
  check-buffer format-buffer tag test compile debug disassemble abbrev
  install toggle run profile configure docs jump edit insert)

(defvar nvp-mode-cache (make-hash-table)
  "Store local variables for modes once they have been loaded.")

;; -------------------------------------------------------------------
;;; Helpers

;; alternative macro version to find at compile time -- falls back to this
;;;###autoload
(defun nvp-setup-program (name &optional path)
  "Lookup program in preferable locations before falling back to PATH."
  (and (symbolp name) (setq name (symbol-name name)))
  (and path (setq path (substitute-env-in-file-name path)))
  (or (nvp:with-gnu/w32
          (cl-loop for p in (delq nil (cons path nvp-program-search-paths))
                   do (let ((f (expand-file-name name p)))
                        (and (file-exists-p f)
                             (file-executable-p f)
                             (cl-return f))))
        (bound-and-true-p (intern (nvp:w32-program name))))
      (executable-find name)))

;;;###autoload
(defun nvp-setup-smie-bindings ()
  "Locally override minor mode bindings when smie functions are available."
  (with-eval-after-load 'smartparens
    (nvp:use-minor-mode-overriding-map 'smartparens-mode
      :after-load 'smie
      :predicate (not (null smie-closer-alist))
      ("C-M-f"    . smie-forward-sexp-command)
      ("C-M-b"    . smie-backward-sexp-command)
      ("<f2> q c" . smie-close-block))))

;; ------------------------------------------------------------
;;; Setup

;;;###autoload
(defun nvp-setup-package-root (pkg)
  "Return a guess for the package root directory."
  (let* ((sym (and pkg (if (stringp pkg) (intern-soft pkg) pkg)))
         (str (and pkg (if (stringp pkg) pkg (symbol-name pkg))))
         (path                          ;path to package
          (cond
           ((and (stringp str)
                 (let ((dir (expand-file-name str nvp/modes)))
                   (cond
                    ((file-exists-p dir) dir) ;should just be here
                    ((file-exists-p str) str)
                    (t ;; look for package directory
                     (locate-file str load-path nil
                                  (lambda (f) (if (file-directory-p f) 'dir-ok))))))))
           ;; should be able to find features and autoloads
           ((and sym (featurep sym) (locate-library str)))
           ;; autoload / already loaded
           ((and sym (ignore-errors (locate-library (symbol-file sym)))))
           (t nil))))
    ;; return directory
    (if path
        (if (file-directory-p path) path
          (directory-file-name (file-name-directory path))))))

(eval-when-compile
  (defsubst nvp:setup-normalize-mode (mode &optional major)
    (setq mode (if mode (nvp:as-symbol mode) major))
    ;; use the standard mode when remapped,
    ;; eg. use python-mode instead of python-ts-mode
    (if-let ((remap (and mode (rassq mode major-mode-remap-alist))))
        (car remap)
      mode))

  (defmacro nvp:setup-local-hooks (mode-vars)
    "Setq-local mode hook variables."
    (macroexp-progn
     `(,@(cl-loop for hook in nvp-mode-function-hooks
                  for slot = (replace-regexp-in-string
                              "nvp-\\(.*\\)-functions" "\\1" (symbol-name hook))
                  for getter = (intern (concat "nvp-mode-vars-" slot))
                  collect `(setq ,hook (,getter ,mode-vars)))))))

;;;###autoload
(cl-defun nvp-setup-mode
    (name          ; package root dir name
     &rest kwargs  ; rest of KWARGS passed to `nvp-mode-vars-make'
     &key
     mode          ; major-mode or key for hash
     abbr-file     ; file containing mode abbrevs
     snippets-dir  ; snippet dir to use instead of mode name
     dir           ; mode root directory
     abbr-table    ; abbrev table to use for mode
     &allow-other-keys
     &aux args)
  "Register mode vars."
  (setq args (nvp:arglist-remove-kwargs '(:mode :snippets-dir) kwargs))
  (setq mode (nvp:setup-normalize-mode mode))
  (or dir (setq dir (nvp-setup-package-root name)))
  (unless (and dir (file-exists-p dir))
    (user-error "Setup for '%s' failed to find package root" (nvp:as-string name)))
  (unless abbr-file
    (setq abbr-file (ignore-errors (car (directory-files dir t "abbrev-table")))))
  (or abbr-table (setq abbr-table (symbol-name mode)))
  (let* ((yas-dir (or (ignore-errors (car (directory-files dir t "snippets")))
                      nvp/snippet))
         (mode-snips
          (expand-file-name (or snippets-dir (symbol-name mode)) yas-dir)))
    ;; Initialize/load mode stuff
    ;; - ensure load-path
    ;; - load snippets
    ;; - load abbrevs
    (when (file-exists-p yas-dir)
      (unless (member yas-dir yas-snippet-dirs)
        (push yas-dir yas-snippet-dirs)
        (yas-load-directory yas-dir)))
    (cl-pushnew dir load-path :test #'string=)
    (ignore-errors (quietly-read-abbrev-file abbr-file))
    (puthash mode (apply #'nvp-mode-vars-make
                         `( :dir ,dir
                            :snippets ,mode-snips
                            :abbr-file ,abbr-file
                            :abbr-table ,abbr-table
                            ,@args))
             nvp-mode-cache)))
(put 'nvp-setup-mode 'lisp-indent-function 'defun)

(defun nvp-setup--merge (slot elems)
  "Merge ELEMS with inherited values from SLOT."
  (let (res cur)
    (while (setq cur (pop elems))
      (if (symbolp cur)
          (--when-let (gethash cur nvp-mode-cache)
            (setq elems
                  (append elems (cl-struct-slot-value 'nvp-mode-vars slot it))))
        (push cur res)))
    (seq-uniq res #'equal)))
  
;;;###autoload
(cl-defun nvp-setup-local
    (name          ; package root dir name
     &rest kwargs  ; rest of KWARGS passed to `nvp-mode-setup'
     &key
     mode          ; major-mode or key for hash
     override      ; override previous entries
     post-fn       ; function called after setup
     &allow-other-keys
     &aux args)
  "Setup local variables for helper package - abbrevs, snippets, root dir."
  (setq args (nvp:arglist-remove-kwargs '(:override :post-fn) kwargs))
  (setq mode (nvp:setup-normalize-mode mode major-mode)
        nvp-mode-name mode)
  ;; `info-lookup' checks this before tring `major-mode'
  (setq-local info-lookup-mode mode)
  (let ((mvars (gethash mode nvp-mode-cache nil)))
    (when (or override (not mvars))
      (setq mvars (apply #'nvp-setup-mode name `(:mode ,mode ,@args))))
    ;; set local vars
    (pcase-let (((cl-struct nvp-mode-vars snippets abbr-file abbr-table) mvars))
      (setq nvp-mode-snippet-dir snippets
            nvp-local-abbrev-file abbr-file
            nvp-local-abbrev-table abbr-table
            local-abbrev-table
            (symbol-value (intern-soft (concat abbr-table "-abbrev-table")))))
    (nvp:setup-local-hooks mvars)
    (setq-local devdocs-current-docs
                (nvp-setup--merge 'docsets (nvp-mode-vars-docsets mvars))))
  (when post-fn (funcall post-fn)))
(put 'nvp-setup-local 'lisp-indent-function 'defun)

(provide 'nvp-setup)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-setup.el ends here
