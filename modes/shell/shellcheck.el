;;; shellcheck.el --- shellcheck compilation -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Compilation output for .sh and .bat files using shellcheck
;; Provides:
;; - font-locking in the output buffer via `xterm-color'
;; - links b/w shellcheck output and source locations
;; - converts 'SC****' warnings/errors to buttons that link to their documentation
;;
;; TODO:
;; - add function to disable a warning at a given line
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(eval-and-compile (require 'compile))
(require 'help-mode)
(nvp-decls :f (xterm-color-colorize-buffer))

;; use "bats ..." for .bat files
(defvar shellcheck-compile-command '(concat "shellcheck " (buffer-file-name)))

(defconst shellcheck-wiki-url "https://github.com/koalaman/shellcheck/wiki/"
  "Location to link shellcheck warnings/errors to their documentation.")

(defconst shellcheck-compilation-regexp
  '(shellcheck "In \\([^ \t\n]+\\) line \\([0-9]+\\)" 1 2))

(defun shellcheck-disable-this-error ()
  "Disable the shellcheck error containing, or immediately following point.
This interactively adds a shellcheck comment directive in the source."
  (interactive)
  (let* ((compilation-skip-threshold 0)
         (errcode (button-label (button-at (next-button 1))))
         (msg (or (compilation-next-error 0) ; point somewhere in a message
                  (compilation-next-error 1)))
         (loc (compilation--message->loc msg))
         (marker (point-marker))
         (file (caar (compilation--loc->file-struct loc)))
         (line (compilation--loc->line loc)))
    ;; this function will ensure a maker is/has been created for the message,
    ;; and goto the corresponding location in the source
    (with-current-buffer
        (if (bufferp file) file
          (apply #'compilation-find-file
                 marker
                 file
                 (cadr (car (compilation--loc->file-struct loc)))
                 (compilation--file-struct->formats
                  (compilation--loc->file-struct loc))))
      (save-restriction
        (widen)
        (goto-char (point-min))
        (forward-line (1- line))
        (unless (bolp)                     ; point is always at bol anyway?
          (beginning-of-line 1))
        (open-line 1)
        (comment-dwim 1)
        (insert
         (if (string= errcode "SC2096") "shellcheck source=/dev/null"
           (concat "shellcheck disable=" errcode)))))))

(defun shellcheck-filter ()
  "Link shellcheck codes to wiki pages in compilation output."
  (let ((end (point)))
    (save-excursion
      (goto-char compilation-filter-start)
      (while (re-search-forward "SC[0-9]\\{4\\}" end t)
        (help-xref-button
         0 'help-url (concat shellcheck-wiki-url (match-string 0)))))))

(defun shellcheck-kill-buffer ()
  (interactive)
  (nvp-ktb))

(defvar shellcheck-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'shellcheck-kill-buffer)
    (define-key map "d" #'shellcheck-disable-this-error)
    (define-key map (kbd "M-s-n") #'forward-button)
    (define-key map (kbd "M-s-p") #'backward-button)
    map))

(define-compilation-mode shellcheck-mode "Shellcheck"
  "Shellcheck results in compilation mode."
  (make-local-variable 'compilation-error-regexp-alist)
  (make-local-variable 'compilation-error-regexp-alist-alist)
  (setq compilation-error-regexp-alist-alist (list shellcheck-compilation-regexp)
        compilation-error-regexp-alist (list (car shellcheck-compilation-regexp)))
  (setq-local compilation-skip-threshold 0)
  (setq-local compilation-buffer-name-function
              (lambda (_m) (concat "*shellcheck: " (buffer-file-name) "*")))
  (add-hook 'compilation-filter-hook #'shellcheck-filter nil t))

;; -------------------------------------------------------------------
;;; Interface

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
