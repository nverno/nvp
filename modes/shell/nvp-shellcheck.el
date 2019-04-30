;;; nvp-shellcheck.el --- shellcheck compilation -*- lexical-binding: t; -*-

;;; Commentary:

;; Compilation for .sh and .bat files using shellcheck

;;; Code:
(eval-when-compile
  (require 'nvp-macro))
(declare-function xterm-color-colorize-buffer "xterm-color")
(nvp-decls)

(defvar nvp-shellcheck-compile-command '(concat "shellcheck " (buffer-file-name)))

;;;###autoload
(defun nvp-shellcheck ()
  "Check current buffer with shellcheck."
  (interactive)
  (nvp-with-process "shellcheck" :proc-args ((buffer-file-name))
    :on-failure (progn
                  (pop-to-buffer "*shellcheck*")
                  (xterm-color-colorize-buffer)
                  (view-mode))))

;;;###autoload
(defun nvp-shellcheck-compile ()
  "Run shellcheck on current buffer with output to compilation buffer."
  (interactive)
  (let* ((compile-command nvp-shellcheck-compile-command)
         (compilation-buffer-name-function
          #'(lambda (_m) (concat "*shellcheck: " (buffer-file-name) "*")))
         (funcs compilation-finish-functions)
         (kill-func #'(lambda (&rest _ignored)
                        (nvp-set-local-keymap :use t ("q" . kill-this-buffer))
                        ;; reset compilation-finish-functions
                        (setq compilation-finish-functions funcs))))
    ;; (setq compilation-finish-functions kill-func)
    (with-current-buffer (nvp-compile)
      (add-hook 'compilation-finish-functions kill-func nil t))))

(defun nvp-shellcheck-compilation-setup ()
  "Add compilation regexp for shellcheck output."
  (when (not (assoc 'shellcheck compilation-error-regexp-alist-alist))
    (let ((re '(shellcheck "In \\([^ \t\n]+\\) line \\([0-9]+\\)" 1 2)))
      (push (car re) compilation-error-regexp-alist)
      (push re compilation-error-regexp-alist-alist))))

(with-eval-after-load 'compile
  (nvp-shellcheck-compilation-setup))

(provide 'nvp-shellcheck)
;;; nvp-shellcheck.el ends here
