;;; nvp-geiser.el --- Scheme geiser -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
;; (require 'geiser nil t)
(nvp:decls :p "geiser" :f (abbrev-table-put abbrev-table-get))


(defun nvp-scheme-active-implementations ()
  "Available scheme implementations."
  (cl-loop for p in '(racket guile mit-scheme) ; geiser-active-implementations
           for prog = (symbol-name p)
           for it = (or (nvp:program-search prog) (executable-find prog))
           when it
           collect p))

;; Map geiser implementation to name for abbrevs/snippets
(eval-when-compile 
  (defsubst nvp:scheme-implementation ()
    (pcase geiser-impl--implementation
      ('mit 'mit-scheme)
      (x x))))

;; Sync local abbrev table/snippets according to implementation
(defun nvp-geiser-sync-mode (&optional no-yas)
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
    (setq nvp-local-abbrev-table mode
          nvp-mode-name (intern mode))
    (setq local-abbrev-table (and table (symbol-value table)))))

(define-advice geiser-set-scheme (:after (&rest _args) "sync-mode-vars")
  (nvp-geiser-sync-mode))


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
(defun nvp-geiser-repl-init-abbrevs (&optional save-parents)
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
  (remove-hook 'pre-command-hook #'nvp-geiser-repl-init-abbrevs t))

(defun nvp-geiser-eval-last-sexp ()
  "Move to end of current sexp and evaluate."
  (interactive)
  (unless (looking-back ")\\s-*" (line-beginning-position))
    (forward-sexp 1)
    (goto-char (1+ (point))))
  (geiser-eval-last-sexp current-prefix-arg))

;; geiser repl broken end-of-line after history
(defun nvp-geiser-inf-end-of-line ()
  (interactive)
  (let ((inhibit-field-text-motion t))
    (end-of-line)))


(provide 'nvp-geiser)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-geiser.el ends here
