;;; nvp-setup.el --- setup hooks/helpers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-07 02:26:16>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 13 February 2019

;;; Commentary:

;; Mode setup helpers

;;; FIXME: better setup with packages

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (nvp-local-vars)
  (require 'subr-x)
  (require 'nvp-macro)
  (require 'smie))
(require 'nvp)

(cl-defstruct (nvp-mode-vars (:constructor nvp-mode-vars-make)
                             (:copier nil))
  "Store the local variables setup when a mode hook first runs."
  dir snippets abbr-file abbr-table)

(defvar nvp-mode-cache (make-hash-table)
  "Store local variables for modes once they have been loaded.")

;; -------------------------------------------------------------------
;;; Helpers

;; alternative macro version to find at compile time -- falls back to this
;;;###autoload
(defun nvp-setup-program (name &optional path)
  "Lookup program in preferable locations before falling back to PATH."
  (and (symbolp name) (setq name (symbol-name name)))
  (and path (substitute-env-in-file-name path))
  (or (nvp-with-gnu/w32
          (cl-loop for p in (delq nil (cons path nvp-program-search-paths))
             do (let ((f (expand-file-name name p)))
                  (and (file-exists-p f)
                       (file-executable-p f)
                       (cl-return f))))
        (bound-and-true-p (intern (nvp-w32-program name))))
      (executable-find name)))

;;;###autoload
(defun nvp-setup-smie-bindings (&optional minor-mode)
  "Locally override minor mode bindings when smie functions are available."
  (nvp-use-minor-mode-overriding-map (or minor-mode 'smartparens-mode)
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

;;;###autoload
(cl-defun nvp-setup-local
    (name
     &key
     mode
     abbr-file
     snippets-dir
     dir
     snippets
     abbr-table
     post-fn)
  "Setup local variables for helper package - abbrevs, snippets, root dir."
  (setq mode (if mode (if (stringp mode) (intern-soft mode) mode) major-mode))
  (let ((mvars (gethash mode nvp-mode-cache nil)))
    (unless mvars
      (or dir (setq dir (nvp-setup-package-root name)))
      (if (not (file-exists-p dir))
          (user-error "Setup for '%s' failed to find package root"
                      (if (symbolp name) (symbol-value name) name))
        (or abbr-file
            (setq abbr-file (ignore-errors
                              (car (directory-files dir t "abbrev-table")))))
        (or snippets
            (setq snippets
                  (concat "snippets/" (or snippets-dir (symbol-name mode)))))
        (or abbr-table (setq abbr-table (symbol-name mode)))
        (setq mvars (nvp-mode-vars-make
                     :dir dir
                     :snippets (expand-file-name snippets dir)
                     :abbr-file abbr-file
                     :abbr-table abbr-table))
        ;; FIXME: should initialize the dir here, loading autoloads etc.
        (cl-pushnew dir load-path :test #'string=)
        (ignore-errors (quietly-read-abbrev-file abbr-file))
        (puthash mode mvars nvp-mode-cache)))
    (setq nvp-snippet-dir (nvp-mode-vars-snippets mvars)
          nvp-abbrev-local-file (nvp-mode-vars-abbr-file mvars)
          nvp-abbrev-local-table (nvp-mode-vars-abbr-table mvars))
    (setq local-abbrev-table
          (symbol-value
           (intern-soft (concat (nvp-mode-vars-abbr-table mvars) "-abbrev-table")))))
  (when post-fn (funcall post-fn)))

(provide 'nvp-setup)
;;; nvp-setup.el ends here
