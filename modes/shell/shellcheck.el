;;; shellcheck.el --- shellcheck compilation -*- lexical-binding: t; -*-

;;; Commentary:

;; Compilation for .sh and .bat files using shellcheck
;; Links shellcheck output to source location and SC**** warnings/errors to wiki

;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'compile)
(require 'help-mode)
(declare-function xterm-color-colorize-buffer "xterm-color")
(nvp-decls)

;; use "bats ..." for .bat files
(defvar shellcheck-compile-command '(concat "shellcheck " (buffer-file-name)))

(defvar shellcheck-compilation-regexp
  '(shellcheck "In \\([^ \t\n]+\\) line \\([0-9]+\\)" 1 2))

(defun shellcheck-filter ()
  "Link shellcheck codes to wiki pages in compilation output."
  (let ((end (point)))
    (save-excursion
      (goto-char compilation-filter-start)
      (while (re-search-forward "SC[0-9]\\{4\\}" end t)
        (help-xref-button
         0 'help-url
         (concat "https://github.com/koalaman/shellcheck/wiki/"
                 (match-string 0)))))))

(defvar shellcheck-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'kill-this-buffer)
    (define-key map (kbd "M-s-n") #'forward-button)
    (define-key map (kbd "M-s-p") #'backward-button)
    map))

(define-compilation-mode shellcheck-mode "Shellcheck"
  "Shellcheck results in compilation mode."
  (make-local-variable 'compilation-error-regexp-alist)
  (make-local-variable 'compilation-error-regexp-alist-alist)
  (setq compilation-error-regexp-alist-alist (list shellcheck-compilation-regexp)
        compilation-error-regexp-alist (list (car shellcheck-compilation-regexp)))
  (setq-local compilation-buffer-name-function
              (lambda (_m) (concat "*shellcheck: " (buffer-file-name) "*")))
  (add-hook 'compilation-filter-hook #'shellcheck-filter nil t))

;;;###autoload
(defun shellcheck-compile ()
  (interactive)
  (compilation-start
   (concat "shellcheck " (buffer-file-name))
   #'shellcheck-mode
   `(lambda (_) (concat "*shellcheck: " (buffer-file-name) "*"))))

;;;###autoload
(defun shellcheck ()
  "Check current buffer with shellcheck."
  (interactive)
  (nvp-with-process "shellcheck" :proc-args ((buffer-file-name))
    :on-failure (progn
                  (pop-to-buffer "*shellcheck*")
                  (xterm-color-colorize-buffer)
                  (view-mode))))

(provide 'shellcheck)
;;; shellcheck.el ends here
