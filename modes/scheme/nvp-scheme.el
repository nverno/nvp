;;; nvp-scheme.el --- scheme helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; - sending REPL regions from different modules:
;;   https://stackoverflow.com/questions/55546058/racket-mode-can-i-evaluate-a-single-form-within-a-given-namespace-at-the-repl
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'geiser nil t)
(nvp:decls :p "geiser" :f (abbrev-table-put abbrev-table-get))

;; determine available scheme implementations
(defun nvp-scheme-active-implementations ()
  (let (res)
    (mapc
     (lambda (i)
       (let ((prog (nvp:as-string i)))
         (--when-let (or (nvp:program-search prog)
                         (executable-find prog))
           (push i res))))
     '(racket guile mit-scheme)) ;; geiser-active-implementations
    res))

;; map geiser implementation to name for abbrevs/snippets
(eval-when-compile 
  (defsubst nvp:scheme-implementation ()
    (pcase geiser-impl--implementation
      ('mit 'mit-scheme)
      (x x))))

;; -------------------------------------------------------------------
;;; Abbrevs

;; Function expansions
;; Don't expand in strings, comments, or function arguments. And don't
;; expand after '-' or '/'.
(defun nvp-scheme-abbrev-fun-expand-p ()
  (when (not (memq last-input-event '(?/ ?- ?\?)))
    (or (memq this-command '(expand-abbrev))
        (nvp:unless-ppss 'soc         ;not in strings/comments
          (save-match-data
            (and (looking-back "([[:alnum:]]*" (line-beginning-position))
                 (save-excursion
                   (goto-char (match-beginning 0))
                   (and
                    ;; don't expand in function/lambda arguments
                    (not (looking-back "(\\(?:lambda\\|define\\) +"
                                       (line-beginning-position))
                         ;; (looking-back
                         ;;  "(define +\\(?:\\sw\\|\\s_\\)+ +"
                         ;;  (line-beginning-position))
                         )
                    (condition-case nil
                        (progn
                          (up-list)
                          (backward-sexp)
                          (not (looking-back "(let\\*? *" (line-beginning-position))))
                      (error t))))))))))

;; variable expansion predicate
(defun nvp-scheme-abbrev-var-expand-p ()
  (not (or (nvp:ppss 'soc)
           (memq last-input-event '(?\? ?- ?/)))))

;; sync local abbrev table/snippets according to implementation
(defun nvp-scheme-sync-mode (&optional no-yas)
  (let* ((scheme (nvp:scheme-implementation))
         (mode (format "%s-mode" scheme))
         (abbr-table (format "%s-abbrev-table" mode))
         (table (intern-soft abbr-table)))
    (setq mode-name (format "λ[%s]" scheme))
    (unless no-yas
      (dolist (scm (nvp-scheme-active-implementations))
        (yas-deactivate-extra-mode
         (intern-soft (concat (symbol-name scm) "-mode"))))
      (yas-activate-extra-mode (intern-soft mode)))
    (setq nvp-abbrev-local-table mode
          nvp-mode-name (intern mode))
    (setq local-abbrev-table (and table (symbol-value table)))))

(define-advice geiser-set-scheme (:after (&rest _args) "sync-mode-vars")
  (nvp-scheme-sync-mode))

;; -------------------------------------------------------------------
;;; Hippie Expand

(defun nvp-scheme-hippie-expand-setup (&optional repl)
  (nvp-he-flex-lisp-setup)
  (setq-local hippie-expand-only-buffers '(scheme-mode racket-mode))
  (when repl
    (nvp-he-history-setup :history 'comint-input-ring
                          :bol-fn 'comint-line-beginning-position
                          :expand-fn 'nvp-he-history-remove-trailing-paren)))

;; -------------------------------------------------------------------
;;; REPL

;;; TODO: `nvp-add-repl'

;; Add to `pre-command-hook' in geiser repl hook.
;; Enables abbrevs according to scheme implementiation
;; since `geiser-impl--implementation' isn't available when this
;; hook is run, the hack here should set the parent abbrev table
;; and remove the enable function the first time expand-abbrev is called
;;
;; This removes previous parent tables unless SAVE-PARENTS is non-nil,
;; in case scheme was changed b/w REPL invocations
(defun nvp-scheme-repl-init-abbrevs (&optional save-parents)
  (let ((impl (nvp:scheme-implementation)))
    (when impl
      (let ((parents (and save-parents
                          (abbrev-table-get local-abbrev-table :parents)))
            (table (intern-soft (concat (symbol-name impl) "-mode-abbrev-table"))))
        (abbrev-table-put
         local-abbrev-table
         :parents (delq nil (cons (symbol-value table) parents))))))
  ;; now remove enable hook/pre-command-hook
  (abbrev-table-put local-abbrev-table :enable-function nil)
  (remove-hook 'pre-command-hook #'nvp-scheme-repl-init-abbrevs t))

;; -------------------------------------------------------------------
;;; Commands

(defun nvp-scheme-eval-last-sexp ()
  "Move to end of current sexp and evaluate."
  (interactive)
  (unless (looking-back ")\\s-*" (line-beginning-position))
    (forward-sexp 1)
    (goto-char (1+ (point))))
  (geiser-eval-last-sexp current-prefix-arg))

;; geiser repl broken end-of-line after history
(defun nvp-scheme-inf-end-of-line ()
  (interactive)
  (let ((inhibit-field-text-motion t))
    (end-of-line)))

(provide 'nvp-scheme)
;;; nvp-scheme.el ends here
