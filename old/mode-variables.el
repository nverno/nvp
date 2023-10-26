;; -*- lexical-binding: t; -*-

;; -------------------------------------------------------------------
;;; Mode variables

;; return MODE value associated with KEY if exists
;; (define-inline nvp-mode-get-val (key &optional mode)
;;   (inline-letevals ((mode (or mode (quote major-mode))) key)
;;     (inline-quote (assq ,key (get ,mode 'nvp)))))

;; return mode value, default to cadr (first value minus the key)
;; (defsubst nvp-mode-val (key &optional all)
;;   (when-let* ((val (nvp-mode-get-val key)))
;;     (if all (cdr val)
;;       (cadr val))))

;; return KEY if defined otherwise lookup its mode value
;; (defsubst nvp-mode-local-or-val (key &optional all)
;;   (or (eval `(bound-and-true-p ,(intern-soft key)))
;;       (nvp-mode-val key all)))
