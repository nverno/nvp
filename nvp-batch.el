;;; nvp-batch.el --- running in batch -*- lexical-binding: t; -*-
;;; Commentary:
;; Run target in batch mode with load-path setup.
;;; Code:

(defvar nvp-batch-packages nil
  "Packages to require in batch mode.")

(defvar nvp-batch-package-env "NVP_BATCH_PKGS"
  "Environment variable to check for list of packages to require.")

(defvar nvp-batch-target-env "NVP_BATCH_TARGET"
  "Environment variable specifying target to call in batch mode.")

(defvar nvp-batch-root
  (file-name-directory
   (cond (load-in-progress load-file-name)
         ((and (boundp 'byte-compile-current-file)
               byte-compile-current-file))
         (t (buffer-file-name)))))

;; load packages
(defun nvp-batch-load-packages ()
  (let ((pkgs (or (getenv nvp-batch-package-env)
                 nvp-batch-packages)))
    (when (stringp pkgs)
      (setq pkgs (mapcar #'intern (split-string pkgs nil t "\\s-*"))))
    (princ (format "Requires: %S\n" pkgs))
    (dolist (pkg (delq nil pkgs))
      (require pkg))))

;;; setup up load-path
(defun nvp-batch-load-path (&optional rootdir)
  (dolist (dir (list (or rootdir default-directory)
                     (expand-file-name "lisp" user-emacs-directory)
                     (expand-file-name "elpa" user-emacs-directory)
                     nvp-batch-root))
    (let ((default-directory dir))
      (normal-top-level-add-subdirs-to-load-path)
      (unless (string-match "elpa" dir)
        (add-to-list 'load-path dir)))))

;;; target
(defun nvp-batch-target ()
  (let* ((target (getenv nvp-batch-target-env))
         (fn (and target (intern target))))
    (unless fn
      (error "%s not %s" nvp-batch-target-env (if target "defined" "set")))
    fn))

(defun nvp-batch-run ()
  "Run target in batch mode with `load-path' and packages setup."
  (nvp-batch-load-path)
  (nvp-batch-load-packages)
  (funcall (nvp-batch-target)))

(provide 'nvp-batch)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-batch.el ends here
