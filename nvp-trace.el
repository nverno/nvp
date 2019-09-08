;;; nvp-trace.el --- trace -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'hydra)
  (require 'nvp-macro))
(require 'trace)
(nvp-auto "find-func" 'read-library-name 'find-library-name)
(nvp-auto "nvp-elisp" 'nvp-elisp-matching-forms)
(nvp-decl flatten-tree)

;;;###autoload(autoload 'nvp-trace-hydra/body "nvp-trace")
(nvp-hydra-set-property 'nvp-trace-hydra)
(defhydra nvp-trace-hydra (:color blue )
  ("f" trace-function "trace func")
  ("b" trace-function-background "trace func background")
  ("u" untrace-function "untrace func")
  ("q" untrace-all "untrace all"))

(defvar nvp-trace-group-alist
  '((popups display-buffer
            pop-to-buffer
            pop-to-buffer-same-window
            switch-to-buffer-other-window
            switch-to-buffer-other-frame)))

(defvar nvp-trace-defun-forms
  '((defun cl-defun declare-function autoload cl-defmethod t)
    (defmacro cl-defmacro)
    (defsubst cl-defsubst)))

;;;###autoload
(defun nvp-trace-group (group)
  "Trace GROUP of functions defined in `nvp-trace-group-alist'."
  (interactive (list (completing-read "Trace group: " nvp-trace-group-alist)))
  (dolist (fn (cdr (assoc (intern group) nvp-trace-group-alist)))
    (trace-function-background fn)))

(defun nvp-untrace-group (group)
  "Untrace functions in GROUP."
  (interactive (list (completing-read "Untrace group: " nvp-trace-group-alist)))
  (dolist (fn (cdr (assoc (intern group) nvp-trace-group-alist)))
    (untrace-function fn)))

;;;###autoload
(defun nvp-trace-buffer ()
  (interactive)
  (view-buffer-other-window trace-buffer))

;;;###autoload
(defun nvp-trace-library (library &optional arg)
  "Trace all top-level defun-like forms in library. 
With prefix, trace macros and substs as well."
  (interactive (list (read-library-name) current-prefix-arg))
  (require 'nvp-elisp)                  ;gather all defun-like forms
  (let* ((def-forms (if arg (flatten-tree nvp-trace-defun-forms)
                      (assoc 'defun nvp-trace-defun-forms)))
         (forms
          (with-temp-buffer
            (insert-file-contents (find-library-name library))
            (with-syntax-table emacs-lisp-mode-syntax-table
              (nvp-elisp-matching-forms def-forms)))))
    (dolist (fn forms)
      (trace-function-background fn))
    (message "tracing: %S" forms)))

(provide 'nvp-trace)
;;; nvp-trace.el ends here
