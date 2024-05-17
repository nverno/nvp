;;; bats-mode.el --- Major mode for Bats buffers -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Major-mode for bats buffers.
;;
;; References
;; - https://github.com/sstephenson/bats
;; - some commands from https://github.com/dougm/bats-mode
;;
;;; Code:
(eval-when-compile (require 'compile))
(require 'treesit)
(require 'sh-script)

(defgroup bats nil
  "Bats programming utilities."
  :group 'languages)

(defcustom bats-command "bats"
  "Command to run bats files."
  :type 'string)

(defcustom bats-indent-offset sh-basic-offset
  "Bats indentation offset."
  :type 'integer
  :safe 'integerp)

(defcustom bats-check-command "batscheck.sh"
  "Command to run batscheck.sh."
  :type 'string)

;;; XXX: used anywhere? for imenu?
(defvar bats-mode-function-re
  (concat "^\\s-*\\(?:"
          ;; function FOO()
          "function\\s-+\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*\\(?:()\\)?"
          ;; FOO()
          "\\|\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*()"
          ;; bats tests
          "\\|@test" "\\)"))


;;; Font-locking

(defvar bats-mode--builtins
  '("load" "run" "skip"
    "bats_load_safe" "bats_load_library" "bats_require_minimum_version"))

(defvar bats-mode--font-lock-keywords
  `(("@test" . font-lock-builtin-face)
    (,(rx-to-string `(group symbol-start (or ,@bats-mode--builtins) symbol-end))
     1 font-lock-builtin-face)))


;;; Compilation
(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist 'bats t)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(bats . ("file \\([^ \t\r\n(]+\\), line \\([0-9]+\\)" 1 2)) t))

;; Just highlight checkmarks
(defun bats-compilation-filter ()
  (save-excursion
    (forward-line 0)
    (let ((end (point)))
      (goto-char compilation-filter-start)
      (forward-line 0)
      (when (< (point) end)
        (setq end (copy-marker end))
        (while (search-forward "✓" end 1)
          (replace-match
           (propertize (match-string 0) 'face 'bold 'font-lock-face 'compilation-info)
           t t))))))

;; align results by checkmarks
(defun bats-compilation-finish (&rest _)
  (let ((inhibit-read-only t))
   (align-regexp (point-min) (point-max) "\\(\\s-+\\)✓" 1)))

;; mark current test, keep marking successive tests when called
;; repeatedly
;; (defun bats-mark-test ()
;;   (interactive)
;;   (if (or (and (eq last-command this-command) (mark t))
;;           (and transient-mark-mode mark-active))
;;       (set-mark
;;        (save-excursion
;;          (goto-char (mark))
;;          (bats-next-test)
;;          (bats--end-of-test)))
;;     (let ((start (bats--beginning-of-test)))
;;       (when start
;;         (bats--end-of-test)
;;         (push-mark nil t t)
;;         (goto-char start)))))

;;; from https://github.com/dougm/bats-mode
(defun bats-mode--current-test ()
  "Find current bats test."
  (let (test-name)
    (save-excursion
      (end-of-line)
      (unless (search-backward-regexp "^@test \"\\(.*?\\)\" {" nil t)
        (error "Unable to find a @test"))
      (setq test-name (match-string 1)))
    test-name))

;; FIXME: Doesn't look like there is a BATS_TEST_PATTERN anymore
(defun bats-run (file &optional name)
  (let ((cmd (concat bats-command " -p " file)))
    (with-current-buffer
        (compile
         (if name
             (concat (format "BATS_TEST_PATTERN='^%s$' " name) cmd)
           cmd))
      (add-hook 'compilation-filter-hook #'bats-compilation-filter nil t)
      (add-hook 'compilation-finish-functions #'bats-compilation-finish nil t))))

(defun bats-run-current-test ()
  (interactive)
  (bats-run-current-file (bats-mode--current-test)))

(defun bats-run-current-file (&optional name)
  (interactive)
  (if buffer-file-name
      (bats-run buffer-file-name name)
    (user-error "Buffer not associated with a file")))

(defun bats-run-all ()
  (interactive)
  (bats-run "."))

(defvar-keymap bats-mode-map
  :doc "Keymap for Bats mode."
  "C-c C-b" #'bats-run-current-file
  "C-c C-a" #'bats-run-all
  "C-c C-c" #'bats-run-current-test)


;;;###autoload
(define-derived-mode bats-mode sh-mode "Bats"
  "Major mode for Bats buffers.

Commands:
\\{bats-mode-map}"
  (setq-local sh-shell "bash")
  (setq-local sh-basic-offset bats-indent-offset)
  (font-lock-add-keywords 'bats-mode bats-mode--font-lock-keywords)
  (setq-local compile-command '(concat bats-check-command " " (buffer-file-name))))


;;; Tree-sitter

(defvar bats-mode--treesit-settings
  (append
   sh-mode--treesit-settings
   (treesit-font-lock-rules
    :feature 'bats
    :language 'bash
    :override t
    `((command
       name: (command_name
              ((word) @font-lock-preprocessor-face)
              (:match "\\`@test'" @font-lock-preprocessor-face)))
      (command
       name: (command_name
              (word) @font-lock-builtin-face
              (:match ,(rx-to-string `(seq bos (or ,@bats-mode--builtins) eos))
                      @font-lock-builtin-face)))))))

(defvar-keymap bats-ts-mode-map
  :doc "Keymap for Bats ts mode."
  :parent (make-composed-keymap bats-mode-map bash-ts-mode-map))


;;;###autoload
(define-derived-mode bats-ts-mode bash-ts-mode "Bats"
  "Major mode for Bats buffers using tree-sitter.

Commands:
\\{bats-ts-mode-map}"
  :syntax-table sh-mode-syntax-table
  (when (treesit-ready-p 'bash)
    (sh-set-shell "bash")
    (setq-local sh-basic-offset bats-indent-offset)
    ;; FIXME: added features not being added properly
    (setq-local treesit-font-lock-settings bats-mode--treesit-settings)
    (treesit-font-lock-recompute-features '(bats) nil 'bash)))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bats\\'" . bats-mode))

(provide 'bats-mode)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; bats-mode.el ends here
