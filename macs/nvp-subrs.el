;;; nvp-subrs.el ---  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; - some helpers from use-package
;; - general simple subrs
;;; Code:
(require 'cl-lib)
(require 'dash)

;;; Conversion
(defsubst nvp-as-symbol (string-or-symbol)
  (declare (pure t) (side-effect-free t))
  (if (symbolp string-or-symbol) string-or-symbol
    (intern string-or-symbol)))

(defsubst nvp-as-string (string-or-symbol)
  (declare (pure t) (side-effect-free t))
  (if (stringp string-or-symbol) string-or-symbol
    (symbol-name string-or-symbol)))

(defsubst nvp-pair-p (x car-pred cdr-pred)
  "Return non-nil if X is a cons satisfying predicates applied to elems."
  (declare (pure t) (side-effect-free t))
  (and (consp x)
       (funcall car-pred (car x))
       (funcall cdr-pred (cdr x))))

(defsubst nvp-dotted-pair-p (x)
  (declare (pure t) (side-effect-free t))
  (and (consp x) (not (consp (cdr x)))))

;; -------------------------------------------------------------------
;;; Lists

(defsubst nvp-list-split-into-sublists (lst n)
  "Split LST into N length sublists."
  (declare (pure t) (side-effect-free t))
  (cl-loop for i from 0 to (1- (length lst)) by n
     collect (butlast (nthcdr i lst) (- (length lst) (+ n i)))))

(defsubst nvp-longest-item (&rest items)
  "Return longest item by `length'."
  (declare (pure t) (side-effect-free t))
  (cl-reduce (lambda (a b) (if (> (length a) (length b)) a b)) items))

(defsubst nvp-listify (&rest args)
  "Ensure all items in ARGS are lists."
  (declare (pure t) (side-effect-free t))
  (mapcar (lambda (arg)
            (and (stringp arg) (setq arg (intern-soft arg)))
            (unless (and arg
                         (listp arg)
                         (not (functionp arg)))
              (setq arg (list arg))))
          args))

(defsubst nvp-list-concat (&rest elems)
  "Remove empty lists from ELEMS and append."
  (declare (pure t) (side-effect-free t))
  (apply #'append (delete nil (delete (list nil) elems))))

(defsubst nvp-list-unquote (args)
  "Unquote, unfunction, all elements in args - return as list.
eg. '(#'a b 'c) => '(a b c), or #'fn => '(fn), or ('a #'b) => '(a b)."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe args) '(function quote))
    (setq args (cadr args)))
  (delq nil (if (listp args)
                (cl-loop for arg in args
                   do (while (memq (car-safe arg) '(function quote))
                        (setq arg (cadr arg)))
                   collect arg)
              (cons args nil))))

(defsubst nvp-list-split-at (pred xs)
  "Return list with first element being elements of LST before PRED was non-nil."
  (let ((ys (list nil)) (zs (list nil)) flip)
    (cl-dolist (x xs)
      (if flip (nconc zs (list x))
        (if (funcall pred x)
            (progn
              (setq flip t)
              (nconc zs (list x)))
          (nconc ys (list x)))))
    (cons (cdr ys) (cdr zs))))

;; -------------------------------------------------------------------
;;; Plists

(defsubst nvp-plist-delete (plist prop)
  (declare (pure t) (side-effect-free t))
  (cl-loop for (k v) on plist by #'cddr
     unless (eq prop k)
     nconc (list k v)))

(defsubst nvp-plist-merge (a b)
  (nconc a (cl-loop for (k v) on b by #'cddr
              if (not (plist-member a k))
              nconc (list k v))))

(defsubst nvp-separate-keywords (lst)
  (let (kws res)
    (while lst
      (if (keywordp (car lst))
          (setq kws (cons (car lst) (cons (cadr lst) kws))
                lst (cddr lst))
        (push (car lst) res)
        (setq lst (cdr lst))))
    (cons kws res)))

;; -------------------------------------------------------------------
;;; Files

(defsubst nvp-ext (&optional path)
  (file-name-extension (or path (buffer-file-name))))
(defsubst nvp-no-ext (&optional path)
  (file-name-sans-extension (or path (buffer-file-name) (buffer-name))))
(defsubst nvp-bfn () (nvp-path 'bfs nil :or-name t))
(defsubst nvp-bfn-no-ext () (nvp-path 'bfse nil :or-name t))
(defsubst nvp-dfn () (nvp-path 'ds))
(defsubst nvp-fn (&optional path)
  (file-name-nondirectory (directory-file-name (or path (buffer-file-name)))))

(provide 'nvp-subrs)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-subrs.el ends here
