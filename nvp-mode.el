;;; nvp-mode.el --- Mode menu -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'transient)
(require 'nvp)
(nvp:decls :p (flycheck) :v (flycheck-mode))
(nvp:auto "nvp-dev" nvp-dev-describe-mode)

(defun nvp-mode--choose (var &optional default)
  (let ((val (eval var)))
    (cond
     ((null val)
      (or default
          (user-error "No '%S' registered" (symbol-name var))))
     ((length= val 1) (car val))
     (t (intern (completing-read (concat (symbol-name var) ": ") val nil t))))))

;;;###autoload
(defun nvp-check-buffer ()
  (interactive)
  (setq prefix-arg current-prefix-arg)
  (--if-let (ignore-errors (nvp-mode--choose 'nvp-check-buffer-functions))
      (call-interactively it)
    (unless flycheck-mode
      (flycheck-mode 1))
    (call-interactively nvp-check-buffer-default-function)))

;;;###autoload
(defun nvp-format-buffer ()
  (interactive)
  (setq prefix-arg current-prefix-arg)
  (call-interactively
   (nvp-mode--choose
    'nvp-format-buffer-functions nvp-format-buffer-default-function)))

;;;###autoload
(defun nvp-debug ()
  (interactive)
  (setq prefix-arg current-prefix-arg)
  (call-interactively
   (nvp-mode--choose 'nvp-debug-functions nvp-debug-default-function)))

;;;###autoload
(defun nvp-test ()
  (interactive)
  (setq prefix-arg current-prefix-arg)
  (call-interactively
   (nvp-mode--choose 'nvp-test-functions nvp-test-default-function)))

;;;###autoload
(defun nvp-tag ()
  (interactive)
  (setq prefix-arg current-prefix-arg)
  (call-interactively
   (nvp-mode--choose 'nvp-tag-functions nvp-tag-default-function)))

;;;###autoload
(defun nvp-disassemble ()
  (interactive)
  (setq prefix-arg current-prefix-arg)
  (call-interactively
   (nvp-mode--choose 'nvp-disassemble-functions nvp-disassemble-default-function)))

;;;###autoload
(defun nvp-abbrev ()
  (interactive)
  (setq prefix-arg current-prefix-arg)
  (call-interactively (nvp-mode--choose 'nvp-abbrev-functions)))

;;;###autoload
(defun nvp-install ()
  (interactive)
  (setq prefix-arg current-prefix-arg)
  (when nvp-mode-install-targets
    (--> (completing-read "Install: " nvp-mode-install-targets nil t)
         (user-error "unimplemented: %s" it))))

;;;###autoload
(defun nvp-toggle ()
  (interactive)
  (setq prefix-arg current-prefix-arg)
  (call-interactively (nvp-mode--choose 'nvp-toggle-functions)))

;;;###autoload
(defun nvp-docs ()
  (interactive)
  (setq prefix-arg current-prefix-arg)
  (call-interactively (nvp-mode--choose 'nvp-docs-functions)))

;;;###autoload
(defun nvp-run ()
  (interactive)
  (setq prefix-arg current-prefix-arg)
  (call-interactively (nvp-mode--choose 'nvp-run-functions)))

;; (cl-defgeneric nvp-disassemble-doc ()
;;   "Return docstring with disassembly."
;;   (user-error "`nvp-disassemble-doc' not implemented for %S" major-mode))
;; (cl-defmethod nvp-disassemble-doc
;;   (&context ((not (null (derived-mode-p 'emacs-lisp-mode 'inferior-emacs-lisp-mode)))
;;              (eql t)))
;;   (message "BLAH"))
;; (defun nvp-disassemble-popup ()
;;   (interactive)
;;   (let ((doc (nvp-disassemble-doc)))
;;     (nvp:with-toggled-tip doc)))

;;; TODO: with some prefix, prompt to choose mode
;;;###autoload(autoload 'nvp-mode-menu "nvp-mode")
(transient-define-prefix nvp-mode-menu ()
  "Mode"
  [["Run"
    ("r" "Run" nvp-run :if-non-nil nvp-run-functions)
    ("c" "Compile" nvp-compile)
    ("t" "Test" nvp-test)]
   ["Edit"
    ("a" "Abbrev" nvp-abbrev :if-non-nil nvp-abbrev-functions)
    ("t" "Toggle" nvp-toggle :if-non-nil nvp-toggle-functions)]
   ["Debug"
    ("d" "Debug" nvp-debug :if-non-nil nvp-debug-functions)
    ("D" "Disassemble" nvp-disassemble :if-non-nil nvp-disassemble-functions)]
   ["Format"
    ("F" "Format buffer" nvp-format-buffer)
    ("L" "Lint buffer" nvp-check-buffer)]
   ["Other"
    ("T" "Tag" nvp-tag)
    ("i" "Install" nvp-install :if-non-nil nvp-mode-install-targets)]
   ["Help"
    ("s" "Search docs" nvp-docs :if-non-nil nvp-docs-functions)
    ("M-?" "Describe mode" nvp-dev-describe-mode :transient t)]])

(provide 'nvp-mode)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-mode.el ends here
