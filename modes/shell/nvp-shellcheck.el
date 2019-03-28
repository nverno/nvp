;;; nvp-shellcheck.el --- shellcheck compilation -*- lexical-binding: t; -*-

;; Last modified: <2019-03-27 23:38:42>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/shell-tools
;; Created: 24 January 2019

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro))
(declare-function xterm-color-colorize-buffer "xterm-color")
(declare-function nvp-compile "nvp-compile")

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
  (let* ((compile-command (concat "shellcheck " (buffer-file-name)))
         (compilation-buffer-name-function
          #'(lambda (_m) (concat "*shellcheck: " (buffer-file-name) "*")))
         (funcs compilation-finish-functions)
         (kill-func #'(lambda (&rest _ignored)
                        (nvp-set-local-keymap :use t ("q" . kill-this-buffer))
                        ;; reset compilation-finish-functions
                        (setq compilation-finish-functions funcs))))
    (setq compilation-finish-functions kill-func)
    (nvp-compile)))

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
