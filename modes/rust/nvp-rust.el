;;; nvp-rust.el --- rustls -*- lexical-binding: t; -*-
;;; Commentary:
;; TODO:
;; - macroexp: rustc --pretty expanded, or rustc --pretty expanded,hygiene
;;   https://doc.rust-lang.org/book/macros.html#debugging-macro-code
;; - deps
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'rustic nil t)
(nvp:decls :p (rustic lsp-rust toml lsp)
           :f ( lsp-rust-analyzer--related-tests lsp-rust-analyzer-related-tests
                lsp--render-element lsp-rust-analyzer-open-external-docs))
(nvp:auto "rustic-cargo" rustic-cargo--get-test-target)

(with-eval-after-load 'rust-ts-mode
  (require 'nvp-rust-ts))

(nvp:defmethod nvp-newline-dwim-comment (syntax arg)
  :modes (rust-mode rust-ts-mode rustic-mode)
  (nvp-newline-dwim--comment syntax arg " * "))

;;; Help-at-point
(cl-defmethod nvp-hap-lsp-search-remote
  ((_server_id (eql rust-analyzer)) &optional _arg)
  (lsp-rust-analyzer-open-external-docs))

;;; Type signature
;; Fixes eldoc documentation showing only '// size =... align =...
;; instead of any useful type information
(cl-defmethod lsp-clients-extract-signature-on-hover
  (contents (_server-id (eql rust-analyzer)))
  (let ((lines (string-lines
                (string-trim-left (lsp--render-element contents)))))
    (while (and lines (string-match-p "^\\s-*\\(/\\|$\\)" (car lines)))
      (setq lines (cdr lines)))
    (car lines)))

;;; Compilation
;; XXX(5/16/24): https://github.com/brotzeit/rustic/pull/531 still open
;; Fixes error when `compilation--start-time' isn't set in compilation buffer
(define-advice rustic-compilation-setup-buffer
    (:around (orig-fn buf &rest args) "start-time")
  (apply orig-fn buf args)
  (with-current-buffer buf
    (setq compilation--start-time (float-time))))

;;; Rustic Doc
;; Set RUSTUP_HOME correctly so convert script finds std location
(nvp:run-once rustic-doc-setup (:before (&rest _))
  (setenv "RUSTUP_HOME" (-> "rustup show | awk 'NR==2 {print $3}'"
                            (shell-command-to-string)
                            (string-trim))))

(defun nvp-rustic-doc-search (&optional dumb)
  "Wrapper for `rustic-doc-search' to call `rustic-doc-dumb-search' with
prefix DUMB."
  (interactive "P")
  (call-interactively (if dumb #'rustic-doc-dumb-search #'rustic-doc-search)))


;; -------------------------------------------------------------------
;;; Tests

(defvar-local nvp-rust-cargo-test-args "")

(defun nvp-rust--cargo-arguments (&optional prompt arguments)
  (let ((args (or arguments nvp-rust-cargo-test-args)))
    (if prompt
        (setq nvp-rust-cargo-test-args
              (read-from-minibuffer "Cargo test arguments: " args))
      args)))

(defun nvp-rustic-cargo-current-test (&optional prompt arguments)
  "Run \"cargo test\" for the test near point."
  (interactive (list current-prefix-arg))
  (rustic-compilation-process-live)
  (let ((args (nvp-rust--cargo-arguments prompt arguments)))
    (-if-let (test-to-run (concat (rustic-cargo--get-test-target) " " args))
        (with-current-buffer (process-buffer (rustic-cargo-run-test test-to-run))
          (setq-local rustic-test-arguments test-to-run))
      (user-error "Could not find test at point."))))

(defun nvp-rust-move-to-current-fn ()
  "Move point to current function decl if point is function decl."
  (let* ((rustic-cargo-fn-regexp
          ;; redefine to allow "pub" in function decls
          "^\s*\\(?:async\s+\\)?\s-*\\(?:pub\s+\\)?\s*fn\s+\\([^(]+\\)\s*(")
         (fn (rustic-cargo--get-current-line-fn-name))
         pos)
    (save-excursion
      (while (and (not fn) (setq pos (nvp:point 'bul)))
        (goto-char pos)
        (setq fn (rustic-cargo--get-current-line-fn-name))))
    (when fn
      (goto-char (car fn))
      (backward-up-list)
      (forward-word -1)
      (point))))

(defun nvp-rust-current-test-dwim (&optional prompt)
  "Run related test.
Choose test to run by
(1) point is currently in a test, run that.
(2) choose test from known tests related to the symbol at point
(3) try known test related to function containing point
(4) run cargo test"
  (interactive "P")
  (cond ((rustic-cargo--get-test-target)
         (nvp-rustic-cargo-current-test prompt))
        ((and (fboundp 'lsp-rust-analyzer--related-tests)
              (or (lsp-rust-analyzer--related-tests)
                  (save-excursion (and (nvp-rust-move-to-current-fn)
                                       (lsp-rust-analyzer--related-tests)))))
         (call-interactively #'lsp-rust-analyzer-related-tests))
        (t (rustic-cargo-test prompt))))

(provide 'nvp-rust)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-rust.el ends here
