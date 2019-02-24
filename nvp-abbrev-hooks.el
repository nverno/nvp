;;; nvp-abbrev-hooks.el --- abbrev expansion predicates -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-24 15:11:43>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created:  6 February 2019

;;; Commentary:

;; general abbrev hooks
;; - post-insert hook
;; - simple enable-functions
;; - expand hooks
;; - abbrev-edit hook
;;
;; Additional
;; - function to grab previous abbrev

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'nvp-macro))
(require 'nvp)
(declare-function expand-abbrev-hook "expand")

;;;###autoload
(defun nvp-abbrev-grab ()
  "Grabs previous symbol if point is at the end of a symbol or if the \
`last-input-event' was a space."
  (if (or (and (characterp last-input-event)
               (eq ? (char-syntax last-input-event)))
          (looking-at-p "\\_>"))
      (let ((str (buffer-substring-no-properties
                  (point) (save-excursion (skip-syntax-backward "w_") (point)))))
        ;; so eldoc still displays args, let it know this was a
        ;; `self-insert-command'
        (if (not (equal "" str)) str
          (prog1 nil (setq this-command 'self-insert-command))))))

;; -------------------------------------------------------------------
;;; Post insert
;; C level only calls `expand-abbrev' when preceding char is word syntax
;; so hook into `post-self-insert-hook'
;;;###autoload
(defun nvp-abbrev-expand-after-symbols-hook ()
  (and (memq (car (syntax-after (1- (point)))) '(0 3))
       (setq this-command 'nvp-abbrev-expand-after-symbols)
       (expand-abbrev)))

;; -------------------------------------------------------------------
;;; Expand Hooks

;; allow abbrevs to expand inside parens
;;;###autoload
(defun nvp-abbrev-expand-in-paren-hook ()
  (cl-letf (((symbol-function 'eolp)
             #'(lambda () (not (eq (char-syntax (char-after)) ?w)))))
    (expand-abbrev-hook)))

;; -------------------------------------------------------------------
;;; Expansion predicates

;; dont expand in strings/comments
;;;###autoload
(defun nvp-abbrev-expand-p ()
  (let ((ppss (syntax-ppss)))
    (not (or (elt ppss 3) (elt ppss 4)))))

;; don't expand in strings/comments or after [_.-:]
;;;###autoload
(defun nvp-abbrev-expand-not-after-punct-p ()
  (and (not (memq last-input-event '(?_ ?. ?- ?:)))
       (let ((ppss (syntax-ppss)))
         (not (or (elt ppss 3) (elt ppss 4))))))

;; -------------------------------------------------------------------
;;; Abbrev table modes

;;;###autoload
(define-derived-mode abbrev-table-mode emacs-lisp-mode "Abbrev-Table"
  "Simple abbrev table extension mode."
  :abbrev-table nil
  (setq-local imenu-generic-expression '((nil "^(define-abbrev-table '\\(.*\\))" 1))))

;;;###autoload
(defun nvp-abbrev-edit-hook ()
  (setq-local imenu-generic-expression '((nil "^(\\(.*\\))" 1))))
;;;###autoload(add-hook 'edit-abbrevs-mode-hook #'nvp-abbrev-edit-hook)

(provide 'nvp-abbrev-hooks)
;;; nvp-abbrev-hooks.el ends here
