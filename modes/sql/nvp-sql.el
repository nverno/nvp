;;; nvp-sql ---  -*- lexical-binding: t; -*- 
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'sql)
(nvp:decls :p (yas) :v (nvp-mode-name))


(with-eval-after-load 'nvp-repl
  (require 'nvp-sql-repl))

;;; Sql Products
;; Sync abbrevs/snippets/etc. when changing b/w different sql products
(defun nvp-sql-change-product (old-product new-product)
  (let* ((prev-mode (intern-soft (format "sql-%s-mode" old-product)))
         (new-mode-name (format "sql-%s-mode" new-product))
         (new-mode (intern new-mode-name))
         (new-abbr-table (format "%s-abbrev-table" new-mode-name)))
    (setq nvp-mode-name new-mode)
    (--when-let (intern-soft new-abbr-table)
      (setq nvp-local-abbrev-table new-mode-name
            local-abbrev-table (symbol-value it)))
    (--when-let (intern-soft prev-mode)
      (yas-deactivate-extra-mode it))
    (yas-activate-extra-mode new-mode)))

(define-advice sql-set-product (:around (orig-fn product) "sync-mode-vars")
  (let ((old-product sql-product))
    (apply orig-fn (list product))
    (nvp-sql-change-product old-product product)))

;;; Sqlite
(defconst nvp-sql-sqlite-font-lock-keywords
  (eval-when-compile
    (list
     ;; missing window functions
     (sql-font-lock-keywords-builder
      'font-lock-builtin-face nil
      "rank" "dense_rank" "cume_dist" "lag")
     ;; OVER clause
     (sql-font-lock-keywords-builder
      'font-lock-builtin-face nil
      "partition" "over" "preceding" "difference"))))

;;; MySQL
(defconst nvp-sql-mysql-font-lock-keywords
  (eval-when-compile
    (list (sql-font-lock-keywords-builder
           'font-lock-builtin-face nil
           "least" "greatest"))))

(defun nvp-sql--add-keywords (product)
  (let ((new-kwds (symbol-value
                   (intern-soft (format "nvp-sql-%s-font-lock-keywords" product))))
        (kwds (intern-soft (format "sql-mode-%s-font-lock-keywords" product))))
    (when (and new-kwds kwds
               (not (cl-member (car new-kwds) (symbol-value kwds) :test #'equal)))
      (set kwds (append new-kwds (symbol-value kwds))))))

(cl-eval-when (load)
  (dolist (product '("sqlite" "mysql"))
    (nvp-sql--add-keywords product)))

;;; Format
(nvp:decl sqlformat-region sqlformat-buffer)
(defun nvp-sql-format-dwim (&optional arg)
  "Format buffer using sqlformat.
The region to format is determined by 1) active region, 2) buffer with single
prefix ARG, or 3) paragraph at point."
  (interactive (let ((arg current-prefix-arg))
                 (nvp:prefix-shift -1)
                 (list arg)))
  (if (region-active-p)
      (call-interactively #'sqlformat-region)
    (let ((args (list current-prefix-arg)))
      (apply (cond ((equal '(4) arg) #'sqlformat-buffer)
                   (t (let ((bnds (bounds-of-thing-at-point 'paragraph)))
                        (nvp-indicate-pulse-region-or-line
                         (car bnds) (1+ (cdr bnds)))
                        (push (cdr bnds) args)
                        (push (car bnds) args))
                      #'sqlformat-region))
             args))))

;;; SQLup
;; modified `sqlup-maybe-capitalize-symbol' to not upcase words after '.'
(nvp:decl sqlup-work-on-symbol)

(defun nvp-sqlup-maybe-capitalize-symbol (direction)
  "DIRECTION is either 1 for forward or -1 for backward"
  (with-syntax-table (make-syntax-table sql-mode-syntax-table)
    ;; Give \ symbol syntax so that it is included when we get a symbol. This is
    ;; needed so that \c in postgres is not treated as the keyword C.
    (modify-syntax-entry ?\\ "_")
    (forward-symbol direction)
    (let ((bnds (bounds-of-thing-at-point 'symbol)))
      (when (and bnds (not (eq ?. (char-before (car bnds))))) 
        (sqlup-work-on-symbol
         (buffer-substring-no-properties (car bnds) (cdr bnds)) bnds)))))

(with-eval-after-load 'sqlup-mode
  (advice-add 'sqlup-maybe-capitalize-symbol
              :override #'nvp-sqlup-maybe-capitalize-symbol))

(provide 'nvp-sql)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-sql.el ends here
