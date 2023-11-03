;;; nvp-elisp-check.el --- Check elisp code -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp:decls :p (checkdoc package-lint))

(put 'nvp-elisp-checkdoc-style 'error-conditions
     '(nvp-elisp-checkdoc-style error))
(put 'nvp-elisp-checkdoc-style 'error-message "Checkdoc bad style")

(defun nvp@elisp-checkdoc-do-error (orig-fn &rest args)
  (let ((errs checkdoc-pending-errors))
    (apply orig-fn args)
    (and errs (signal 'nvp-elisp-checkdoc-style nil))))

;;;###autoload
(defun nvp-elisp-check-buffer ()
  "Check buffer with `checkdoc' and `package-lint', when available."
  (interactive)
  (and (not (or (nvp-elisp-checkdoc)
                (nvp-elisp-package-lint)))
       (nvp-indicate-modeline "All good" 'success)))

(defun nvp-elisp-checkdoc (&optional indicate)
  (interactive)
  (let (did-error)
    (unwind-protect
        (condition-case err
            (progn
              (advice-add 'checkdoc-show-diagnostics :around #'nvp@elisp-checkdoc-do-error)
              (checkdoc-current-buffer 'take-notes)
              (--when-let (get-buffer checkdoc-diagnostic-buffer)
                (kill-buffer it))
              (and indicate (nvp-indicate-modeline "All good" 'success)))
          (user-error
           (advice-remove 'checkdoc-show-diagnostics #'nvp@elisp-checkdoc-do-error)
           (message (error-message-string err))
           (call-interactively #'checkdoc))
          (nvp-elisp-checkdoc-style
           (setq did-error t)
           (message (error-message-string err))))
      (advice-remove 'checkdoc-show-diagnostics #'nvp@elisp-checkdoc-do-error))
    did-error))

(defun nvp-elisp-package-lint (&optional indicate)
  (interactive)
  (when (fboundp 'package-lint-buffer)
    (let ((file (file-name-nondirectory (buffer-file-name)))
          (errs (package-lint-buffer)))
      (if errs
          (with-current-buffer (get-buffer-create "*Package-Lint*")
            (let ((inhibit-read-only t))
              (erase-buffer))
            (font-lock-mode)
            (insert "Package Lint:\n\n")
            (compilation-minor-mode)
            (pcase-dolist (`(,line ,col ,type ,message) errs)
              (insert (format "%s:%d:%d: %s: %s\n" file line col type message)))
            (goto-char (point-min))
            (forward-line 2)
            (compilation--ensure-parse (point))
            (setq compilation-current-error (point))
            (pop-to-buffer (current-buffer))
            errs)
        (--when-let (get-buffer "*Package-Lint*")
          (kill-buffer it))
        (and indicate (nvp-indicate-modeline "All good" 'success))
        nil))))

(provide 'nvp-elisp-check)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-elisp-check.el ends here
