;;; nvp-setup.el --- setup hooks/helpers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-05 16:51:10>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 13 February 2019

;;; Commentary:

;; Mode setup helpers

;;; FIXME: check for package on load-history before running setup routine
;; should cache packages w/ local variables as structs
;; - only search in setup when not loaded
;;   `load-history-regexp', `load-history-filename-element'
;; - use mode-local ?, better way to set many mode-local variables

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (nvp-local-vars)
  (require 'subr-x)
  (require 'nvp-macro)
  (require 'smie))
(require 'nvp)

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
;; FIXME: better way to implement these

;;;###autoload
(defun nvp-setup-package-root (pkg)
  "Return a guess for the package root directory."
  (let* ((sym (and pkg (if (stringp pkg) (intern-soft pkg) pkg)))
         (str (and pkg (if (stringp pkg) pkg (symbol-name pkg))))
         (path                          ;path to package
          (cond
           ;; should be able to find features and autoloads
           ((and sym (featurep sym) (locate-library str)))
           ((and sym                    ;autoload / already loaded
                 (ignore-errors (locate-library (symbol-file sym)))))
           ;; check if pkg exists or is on load-path
           ((and (stringp str)
                 (if (file-exists-p pkg) pkg
                   ;; look for package directory
                   (locate-file str load-path nil
                                (lambda (f) (if (file-directory-p f) 'dir-ok))))))
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
     (dir (nvp-setup-package-root name))
     (snippets (concat "snippets/" (or snippets-dir mode (symbol-name major-mode))))
     (abbr-table (or mode (symbol-name major-mode)))
     (fn nil))
  "Setup local variables for helper package - abbrevs, snippets, root dir."
  (if (not (file-exists-p dir))
      (user-error "Setup for '%s' failed to find package root"
                  (if (symbolp name) (symbol-value name) name))
    (or abbr-file
        (setq abbr-file (ignore-errors
                          (car (directory-files dir nil "abbrev-table")))))
    (setq-local nvp-snippet-dir (expand-file-name snippets dir))
    (setq-local nvp-abbrev-local-table abbr-table)
    (and abbr-file
         (setq-local nvp-abbrev-local-file (expand-file-name abbr-file dir))
         (ignore-errors (quietly-read-abbrev-file nvp-abbrev-local-file)))
    (setq-local local-abbrev-table
                (symbol-value
                 (intern-soft (concat nvp-abbrev-local-table "-abbrev-table")))))
  (when fn (funcall fn)))

(provide 'nvp-setup)
;;; nvp-setup.el ends here
