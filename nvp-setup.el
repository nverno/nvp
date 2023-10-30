;;; nvp-setup.el --- setup hooks/helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; Mode setup helpers

;;; FIXME: better setup with packages

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'smie))
(require 'nvp)
(require 'yasnippet)
(defvar info-lookup-mode)

(cl-defstruct (nvp-mode-vars (:constructor nvp-mode-vars-make)
                             (:copier nil))
  "Store the local variables setup when a mode hook first runs."
  dir snippets abbr-file abbr-table
  ;; functions
  check-buffer format-buffer tag test compile debug disassemble abbrev
  toggle run profile docs
  ;; installs
  install-targets)

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
  (nvp:use-minor-mode-overriding-map 'smartparens-mode
    :after-load 'smie
    :predicate (not (null smie-closer-alist))
    ("C-M-f"    . smie-forward-sexp-command)
    ("C-M-b"    . smie-backward-sexp-command)
    ("<f2> q c" . smie-close-block)))

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
  (defmacro nvp:setup-local-hooks (mode-vars)
    (macroexp-progn
     `(,@(cl-loop for hook in nvp-mode-function-hooks
                  for slot = (replace-regexp-in-string
                              "nvp-\\(.*\\)-functions" "\\1" (symbol-name hook))
                  for getter = (intern (concat "nvp-mode-vars-" slot))
                  collect `(setq ,hook (,getter ,mode-vars)))))))

;;;###autoload
(cl-defun nvp-setup-local
    (name                               ; package root dir name
     &key
     mode                               ; major-mode or key for hash
     abbr-file                          ; file containing mode abbrevs
     snippets-dir                       ; snippet dir to use instead of mode name
     dir                                ; mode root directory
     abbr-table                         ; abbrev table to use for mode
     override                           ; override previous entries
     post-fn                            ; function called after setup
     ;; functions / hooks
     check-buffer format-buffer tag test compile debug disassemble abbrev
     toggle run profile docs
     install-targets)
  "Setup local variables for helper package - abbrevs, snippets, root dir."
  (setq mode (if mode (nvp:as-symbol mode) major-mode))
  ;; use the standard mode when remapped,
  ;; eg. use python-mode instead of python-ts-mode
  (when-let ((remap (rassq mode major-mode-remap-alist)))
    (setq mode (car remap)
          ;; `info-lookup' checks this before tring `major-mode'
          info-lookup-mode mode))
  (setq nvp-mode-name mode)
  (let ((mvars (gethash mode nvp-mode-cache nil))
        yas-dir mode-snips)
    ;; initialization that happens once
    (unless (and (not override) mvars)
      (nvp:defq dir (nvp-setup-package-root name))
      (if (not (and dir (file-exists-p dir)))
          (user-error
           "Setup for '%s' failed to find package root" (nvp:as-string name))
        (nvp:defq abbr-file
          (ignore-errors (car (directory-files dir t "abbrev-table"))))
        ;; top-level snippets dir to load
        (setq yas-dir (or (ignore-errors (car (directory-files dir t "snippets")))
                          nvp/snippet))
        (setq mode-snips
              (expand-file-name (or snippets-dir (symbol-name mode)) yas-dir))
        (nvp:defq abbr-table (symbol-name mode))
        (setq mvars (nvp-mode-vars-make
                     :dir dir
                     :snippets mode-snips
                     :abbr-file abbr-file
                     :abbr-table abbr-table
                     :check-buffer check-buffer
                     :format-buffer format-buffer
                     :tag tag
                     :test test
                     :compile compile
                     :debug debug
                     :disassemble disassemble
                     :abbrev abbrev
                     :toggle toggle
                     :run run
                     :profile profile
                     :docs docs
                     :install-targets install-targets))
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
        (puthash mode mvars nvp-mode-cache)))
    ;; initialization that happens every time
    (pcase-let (((cl-struct nvp-mode-vars snippets abbr-file abbr-table) mvars))
      (setq nvp-mode-snippet-dir snippets
            nvp-abbrev-local-file abbr-file
            nvp-abbrev-local-table abbr-table
            nvp-mode-install-targets install-targets
            local-abbrev-table
            (symbol-value (intern-soft (concat abbr-table "-abbrev-table")))))
    (nvp:setup-local-hooks mvars))
  (when post-fn (funcall post-fn)))
(put 'nvp-setup-local 'lisp-indent-function 'defun)

;; ;;;###autoload
;; (cl-defun nvp-setup-mode (_mode &key check format test tag compile debug disassemble)
;;   (declare (indent 1))
;;   (pcase-dolist (`(,type ,vals) `((check-buffer ,check)
;;                                   (format-buffer ,format)
;;                                   (test ,test)
;;                                   (tag ,tag)
;;                                   (compile ,compile)
;;                                   (debug ,debug)
;;                                   (disassemble ,disassemble)))
;;     (when vals
;;       (let ((lst (intern (concat "nvp-" (symbol-name type) "-functions"))))
;;         (dolist (v vals)
;;           (add-hook lst v nil t))))))

(provide 'nvp-setup)
;;; nvp-setup.el ends here
