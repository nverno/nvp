;;; nvp-sql ---  -*- lexical-binding: t; -*- 
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp-decls :v (sql-product sql-buffer zeal-at-point-docset))
(declare-function sql-set-product "sql")
(declare-function sql-set-sqli-buffer "sql")
(declare-function sql-product-font-lock "sql")

;; ------------------------------------------------------------
;;; SQLi

;; font-lock everything in sql interactive mode
(defun nvp-sql-sqli-font-lock ()
  (unless (eq 'oracle sql-product)
    (sql-product-font-lock nil nil)))

;; Suppress indentation in sqli.
(defun nvp-sql-sqli-suppress-indent ()
  (set (make-local-variable 'indent-line-function)
       (lambda () 'noindent)))

;; setup repl
(defun nvp-sql-sqli-setup ()
  (nvp-sql-sqli-font-lock)
  (nvp-sql-sqli-suppress-indent)
  (nvp-sql-psql-set-zeal))

(defvar nvp-sql--sql-buffer)

;; Switch to the corresponding sqli buffer.
(defun nvp-sql-sqli-switch ()
  (interactive)
  (if (eq major-mode 'sql-mode)
      (let ((buff (current-buffer)))
        (if sql-buffer
            (progn
              (pop-to-buffer sql-buffer)
              (goto-char (point-max)))
          (sql-set-sqli-buffer)
          (when sql-buffer
            (nvp-sql-sqli-switch)))
        (if sql-buffer
            (setq nvp-sql--sql-buffer buff)
          (user-error "No sqli buffer found.")))
    (when nvp-sql--sql-buffer
      (pop-to-buffer nvp-sql--sql-buffer))))

;;; Zeal

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
