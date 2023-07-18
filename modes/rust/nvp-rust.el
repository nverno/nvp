;;; nvp-rust.el --- rustls -*- lexical-binding: t; -*-
;;; Commentary:
;; TODO:
;; - macroexp: rustc --pretty expanded, or rustc --pretty expanded,hygiene
;;   https://doc.rust-lang.org/book/macros.html#debugging-macro-code
;; - deps
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'rustic nil t)
(nvp:decls :v (toml-mode-map)
           :f (rustic-cargo-current-test
               rustic-cargo--get-test-target rustic-cargo-test
               rustic-cargo--get-current-fn-name rustic-cargo--get-current-mod
               lsp-rust-analyzer--related-tests lsp-rust-analyzer-related-tests))
(nvp:auto "rustic-cargo" rustic-cargo--get-test-target)

(cl-defmethod nvp-newline-dwim-comment
  (syntax arg &context (major-mode rust-mode))
  (nvp-newline-dwim--comment syntax arg " * "))

(cl-defmethod nvp-newline-dwim-comment
  (syntax arg &context (major-mode rustic-mode))
  (nvp-newline-dwim--comment syntax arg " * "))


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

;; -------------------------------------------------------------------
;;; Tests

(defun nvp-rust-current-test-dwim ()
  "Run related test.
Choose test to run by
(1) point is currently in a test, run that.
(2) choose test from known tests related to the symbol at point
(3) try known test related to function containing point
(4) run cargo test" 
  (interactive)
  (cond
   ((rustic-cargo--get-test-target)
    (rustic-cargo-current-test))
   ((and (fboundp 'lsp-rust-analyzer--related-tests)
         (or
          (and (lsp-rust-analyzer--related-tests)
               (call-interactively #'lsp-rust-analyzer-related-tests))
          (save-excursion
            (and (nvp-rust-move-to-current-fn)
                 (and (lsp-rust-analyzer--related-tests)
                      (call-interactively #'lsp-rust-analyzer-related-tests)))))))
   (t (rustic-cargo-test))))

;; -------------------------------------------------------------------
;;; Insert / Toggle

;; rust mode functions:
;; rust-in-comment-paragraph
;; rust-in-macro
;; rust-in-str-or-cmnt
;; rust-beginning-or-defun
;; rust-end-of-defun

;; toggle public visibility of function at point
;; (defun nvp-rust-toggle-pub ()
;;   (interactive)
;;   (save-excursion
;;     (rust-beginning-of-defun)
;;     (if (and (looking-at-p "pub ")
;;              (message "pub off"))
;;         (delete-char 4)
;;       (insert "pub ")
;;       (message "pub on"))))

;; toggle mutability of variable at point
;; (defun nvp-rust-toggle-mut ()
;;   (interactive)
;;   (save-excursion
;;     (racer-find-definition)
;;     (back-to-indentation)
;;     (forward-char 4)
;;     (if (and (looking-at-p "mut ")
;;              (message "mutability off"))
;;         (delete-char 4)
;;       (insert "mut ")
;;       (message "mutability on"))))

;;; Enums / Struct

;; (eval-when-compile
;;   ;; do body business at item location
;;   (defmacro nvp-rust-at-definition-of (item &rest body)
;;     (declare (indent defun))
;;     `(save-excursion
;;        (with-current-buffer (find-file-noselect
;;                              (get-text-property 0 'file ,item))
;;          (goto-char (point-min))
;;          (forward-line (1- (get-text-property 0 'line ,item)))
;;          (forward-char (get-text-property 0 'col ,item))
;;          ,@body)))

;;   ;; collect stuff between beginning/end of rust def at point
;;   (defmacro nvp-rust-collect-fields (regexp)
;;     (declare (indent defun))
;;     `(save-excursion
;;        (rust-beginning-of-defun)
;;        (let ((end (save-excursion
;;                     (rust-end-of-defun)
;;                     (point)))
;;              (case-fold-search)
;;              opts)
;;          (while (< (point) end)
;;            (and (looking-at ,regexp)
;;                 (push (match-string-no-properties 1) opts))
;;            (forward-line 1))
;;          opts))))

(provide 'nvp-rust)
;;; nvp-rust.el ends here
