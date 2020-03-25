;;; shellcheck.el --- shellcheck compilation -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Compilation output for .sh and .bat files using shellcheck
;;
;; Provides:
;; - font-locking in the output buffer via `xterm-color'
;; - links b/w shellcheck output and source locations
;; - converts 'SC****' warnings/errors to buttons that link to their documentation
;; - can disable warnings/errors by adding shellcheck directives from compilation
;;   buffer with `shellcheck-disable-this-error'
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(eval-and-compile (require 'compile))
(require 'help-mode)
(nvp-decls :f (xterm-color-colorize-buffer winner-undo))

;; use "bats ..." for .bat files
(defvar shellcheck-executable (nvp-program "shellcheck"))
(defvar shellcheck-arguments () "Command-line arguments to shellcheck.")

(defconst shellcheck-wiki-url "https://github.com/koalaman/shellcheck/wiki/"
  "Location to link shellcheck warnings/errors to their documentation.")

(defconst shellcheck-compilation-regexp
  '(shellcheck "In \\([^ \t\n]+\\) line \\([0-9]+\\)" 1 2))

(defun shellcheck-compile-command ()
  (concat
   (mapconcat
    #'identity
    (cons shellcheck-executable shellcheck-arguments) " ")
   " " (buffer-file-name)))

(defun shellcheck-disable-this-error ()
  "Disable the shellcheck error containing, or immediately following point.
This interactively adds a shellcheck comment directive in the source."
  ;; Could add prefix argument to save+recompile?
  ;; Saving + recompile is almost always what I want -- there are rare cases
  ;; where the directive is inserted on the wrong line, however, but it's an
  ;; ez undo.
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
           (concat "shellcheck disable=" errcode))))
      (save-buffer))
    (recompile)))

(defun shellcheck-kill-buffer ()
  (interactive)
  (nvp-ktb))

(defvar shellcheck-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q"           #'shellcheck-kill-buffer)
    (define-key map "d"           #'shellcheck-disable-this-error)
    (define-key map (kbd "M-s-n") #'forward-button)
    (define-key map (kbd "M-s-p") #'backward-button)
    map))

(defun shellcheck-filter ()
  "Link shellcheck codes to wiki pages in compilation output."
  (let ((end (point)))
    (save-excursion
      (goto-char compilation-filter-start)
      (while (re-search-forward "SC[0-9]\\{4\\}" end t)
        (help-xref-button
         0 'help-url (concat shellcheck-wiki-url (match-string 0)))))))

(defun shellcheck-compilation-finish (buf msg)
  (when (string-prefix-p "finished" msg)
    (kill-buffer buf)
    (nvp-indicate-modeline "All good" 'success)))

(define-compilation-mode shellcheck-mode "Shellcheck"
  "Shellcheck results in compilation mode.
From `shellcheck-mode' buffers, warning/error messages are linked to both the
source code location and the associated documentation for each code on the wiki.
Calling `shellcheck-disable-this-error' will add the appropriate directive to
the source buffer, save it, and recompile."
  (make-local-variable 'compilation-error-regexp-alist)
  (make-local-variable 'compilation-error-regexp-alist-alist)
  (setq compilation-error-regexp-alist-alist (list shellcheck-compilation-regexp)
        compilation-error-regexp-alist (list (car shellcheck-compilation-regexp)))
  (setq-local compilation-skip-threshold 0)
  (add-hook 'compilation-filter-hook #'shellcheck-filter nil t)
  (add-hook 'compilation-finish-functions #'shellcheck-compilation-finish nil t))

;; -------------------------------------------------------------------
;;; Interface

;;;###autoload
(defun shellcheck-compile ()
  "Compile current buffer with shellcheck.
Output is in `shellcheck-mode' compilation buffer, which see."
  (interactive)
  (compilation-start (shellcheck-compile-command) 'shellcheck-mode))

;;;###autoload
(defun shellcheck ()
  "Check current buffer with shellcheck."
  (interactive)
  (nvp-with-process "shellcheck"
    :proc-args ((buffer-file-name))
    :on-failure (progn
                  (pop-to-buffer "*shellcheck*")
                  (xterm-color-colorize-buffer)
                  (view-mode))))

(provide 'shellcheck)
;;; shellcheck.el ends here
