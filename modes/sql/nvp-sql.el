;;; nvp-sql ---  -*- lexical-binding: t; -*- 
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'sql)
(nvp:decls :v (zeal-at-point-docset))

;;; Sqlite
(defconst nvp-sql-sqlite-font-lock-keywords
  (eval-when-compile
    (list
     ;; missing window functions
     (sql-font-lock-keywords-builder
      'font-lock-builtin-face nil
      "rank" "cume_dist" "lag")
     ;; OVER clause
     (sql-font-lock-keywords-builder
      'font-lock-builtin-face nil
      "partition" "over" "preceding" "difference"))))

(cl-eval-when (load)
  (unless (cl-member (car nvp-sql-sqlite-font-lock-keywords)
                     sql-mode-sqlite-font-lock-keywords :test #'equal)
    (setq sql-mode-sqlite-font-lock-keywords
          (append nvp-sql-sqlite-font-lock-keywords
                  sql-mode-sqlite-font-lock-keywords))))

;; ------------------------------------------------------------
;;; SQLi

;; setup repl
(defun nvp-sql-sqli-setup ()
  ;; font-lock everything in sql interactive mode
  (unless (eq 'oracle sql-product)
    (sql-product-font-lock nil nil))
  ;; Suppress indentation in sqli.
  (set (make-local-variable 'indent-line-function) (lambda () 'noindent))
  (nvp-sql-psql-set-zeal))

(defun nvp-sql-sqli-buffer ()
  (save-window-excursion
    (sql-show-sqli-buffer)
    (get-buffer-process (current-buffer))))

(with-eval-after-load 'nvp-repl
  (nvp-repl-add '(sql-mode)
    :modes '(sql-interactive-mode)
    :wait 0.1
    :find-fn (lambda () (get-buffer sql-buffer))
    :init #'nvp-sql-sqli-buffer))

;; Zeal

;; Default the zeal lookup to postgres when product changes.
(defun nvp-sql-psql-set-zeal ()
  (when (eq sql-product 'postgres)
    (set (make-local-variable 'zeal-at-point-docset) "psql")))

(defadvice sql-set-product (after set-zeal-docset activate)
  (nvp-sql-psql-set-zeal))

;;; Abbrevs

;; Don't expand in strings or comments.
(defun sql-in-code-context-p ()
  (let ((ppss (syntax-ppss)))
    (and (null (elt ppss 3))    ; inside string
         (null (elt ppss 4))))) ; inside comment

(provide 'nvp-sql)
;;; nvp-sql.el ends here
