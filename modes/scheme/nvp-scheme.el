;;; nvp-scheme.el --- scheme helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Stuff common to racket or geiser
;;; Code:

(eval-when-compile (require 'nvp-macro))
(nvp:decls)


;;; Abbrevs
;; Function expansions
;; Don't expand in strings, comments, or function arguments. And don't
;; expand after '-' or '/'.
(defun nvp-scheme-abbrev-fun-expand-p ()
  (when (not (memq last-input-event '(?/ ?- ?\?)))
    (or (memq this-command '(expand-abbrev))
        (nvp:unless-ppss 'soc           ; not in strings/comments
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

;; Variable expansion predicate
(defun nvp-scheme-abbrev-var-expand-p ()
  (not (or (memq last-input-event '(?\? ?- ?/))
           (nvp:ppss 'soc))))


;;; Hippie Expand
(defun nvp-scheme-hippie-expand-setup (&optional repl)
  (nvp-he-flex-lisp-setup)
  (setq-local hippie-expand-only-buffers '(scheme-mode racket-mode))
  (when repl
    (nvp-he-history-setup
     :history 'comint-input-ring
     :bol-fn 'comint-line-beginning-position
     :expand-fn 'nvp-he-history-remove-trailing-paren)))


(provide 'nvp-scheme)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-scheme.el ends here
