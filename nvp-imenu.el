;;; nvp-imenu.el --- imenu helpers  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-02-22 20:11:35>
;; Created: 25 January 2017

;;; Commentary:

;; imenu extensions:
;; - add support for mode specific headers
;; - wrapper function to call imenu/idomenu with additional options
;; - utilities to manipulate index-alist

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'idomenu nil t)
(declare-function idomenu "idomenu")
(autoload 'nvp-comment-start "nvp-comment")
(autoload 'nvp-list-flatten "nvp-util")

;;; Local variables

;; top-level header regexp
(defvar-local nvp-imenu-comment-headers-re nil "Imenu top-level header regexp.")

;; header regexp nested under "Headers"
(defvar-local nvp-imenu-comment-headers-re-1 nil
  "Imenu header regexp nested under 'Headers'")

;; sub-headers
(defvar-local nvp-imenu-comment-headers-re-2 nil "Imenu sub-header regexp.")

;; -------------------------------------------------------------------
;;; Util
;; #<marker at 12739 in which-func.el.gz>
(eval-when-compile
  (defmacro ido/imenu ()
    (if (featurep 'idomenu) '(idomenu) '(imenu))))

(defun nvp-imenu-sort-relative-positions (marker alist)
  "Sort imenu entries so those closest in the buffer are first."
  (cl-sort alist (apply-partially #'nvp-imenu--relative-positions marker)))

(defun nvp-imenu-cleaned-alist (&optional alist)
  "Flatten imenu alist, remove headers and things that don't look like code."
  (cl-remove-if-not
   #'nvp-imenu-maybe-code-p (nvp-list-flatten (or alist imenu--index-alist))))

(defun nvp-imenu-maybe-code-p (elem)
  "Filter out probable non code things."
  (let (mark)
    (and (consp elem)
         (not (string-match-p "[ \t;]\\|:\\'" (car elem)))
         (or (number-or-marker-p (setq mark (cdr elem)))
             (and (overlayp mark)
                  (setq mark (overlay-start mark)))))))

(defun nvp-imenu--relative-positions (pos item1 item2)
  "Return non-nil if ITEM1 is closer to POS than ITEM2 in the buffer.
Assumes the list is flattened and only elements with markers remain."
  (< (abs (- pos (cdr item1)))
     (abs (- pos (cdr item2)))))

;; -------------------------------------------------------------------
;;; Hook

;; make header from comment
;;;###autoload
(cl-defun nvp-imenu-setup (&key headers headers-1 headers-2 extra)
  "Sets up imenu regexps including those to recognize HEADERS and any \
EXTRA regexps to add to `imenu-generic-expression'.
Any extra regexps should be an alist formatted as `imenu-generic-expression'."
  (setq nvp-imenu-comment-headers-re
        (or headers 
            `((nil ,(concat "^" (regexp-quote (nvp-comment-start 3))
                            "\\s-*\\(.*\\)\\s-*$")
                   1))))
  (setq nvp-imenu-comment-headers-re-1
        (or headers-1 `(("Headers" ,(cadr (car nvp-imenu-comment-headers-re))
                         1))))
  (setq nvp-imenu-comment-headers-re-2
        (or headers-2
            `(("Sub-Headers"
               ,(concat "^" (nvp-comment-start 2) "-+\\s-*\\(.*\\)[ -]*$")
               1))))
  (setq-local imenu-generic-expression
              (append imenu-generic-expression
                      extra nvp-imenu-comment-headers-re-1)))

;; -------------------------------------------------------------------
;;; Commands

;;;###autoload
(defun nvp-imenu-idomenu (arg)
  (interactive "P")
  (pcase arg
    (`(4)
     ;; headers only
     (let ((imenu-generic-expression nvp-imenu-comment-headers-re)
           (imenu-create-index-function 'imenu-default-create-index-function))
       (condition-case nil
           (ido/imenu)
         (error (message "nvp-imenu-comment-headers-re: %s"
                         nvp-imenu-comment-headers-re)))))
    (`(16)
     ;; headers + sub-headers
     (let ((imenu-generic-expression
            (append nvp-imenu-comment-headers-re-1
                    nvp-imenu-comment-headers-re-2))
           (imenu-create-index-function 'imenu-default-create-index-function))
       (ido/imenu)))
    (_ (ido/imenu))))

(provide 'nvp-imenu)
;;; nvp-imenu.el ends here
