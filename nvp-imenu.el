;;; nvp-imenu.el --- imenu helpers  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-03-23 18:46:50>
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
(require 'nvp)
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

(defvar nvp-imenu-default-filter-regex (regexp-opt '("Headers" "Sub-Headers"))
  "Regex to match sublist headers to filter out of cleaned imenu alist.")

;; -------------------------------------------------------------------
;;; Util
;; #<marker at 12739 in which-func.el.gz>
(eval-when-compile
  (defmacro ido/imenu ()
    (if (featurep 'idomenu) '(idomenu) '(imenu))))

(defun nvp-imenu-filter-regex (regex &optional alist)
  "Remove entries from ALIST matching REGEX."
  (cl-remove-if (apply-partially #'string-match-p regex) alist :key #'car))

(defun nvp-imenu-sort-relative-positions (marker alist)
  "Sort imenu entries so those closest in the buffer are first."
  (cl-sort alist (apply-partially #'nvp-imenu--relative-positions marker)))

(defun nvp-imenu-cleaned-alist (&optional regex alist)
  "Flatten imenu alist, remove headers and things that don't look like code."
  (or regex (setq regex nvp-imenu-default-filter-regex))
  (or alist (setq alist imenu--index-alist))
  (cl-remove-if-not
   #'nvp-imenu-maybe-code-p
   (nvp-list-flatten (if (and regex (stringp regex))
                         (nvp-imenu-filter-regex regex alist)
                       alist))))

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
  (with-demoted-errors "Error in nvp-imenu-idomenu: %S"
   (pcase arg
     (`(4)                              ; headers only
      (let ((imenu-generic-expression
             (or nvp-imenu-comment-headers-re
                 (symbol-value (nvp-mode-val 'imenu-hdrs))))
            (imenu-create-index-function 'imenu-default-create-index-function))
        (ido/imenu)))
     (`(16)                             ; headers + sub-headers only
      (let ((imenu-generic-expression
             (append (or nvp-imenu-comment-headers-re
                         (symbol-value (nvp-mode-val 'imenu-hdrs)))
                     (or nvp-imenu-comment-headers-re-2
                         (symbol-value (nvp-mode-val 'imenu-hdrs-2)))))
            (imenu-create-index-function 'imenu-default-create-index-function))
        (ido/imenu)))
     (_ (ido/imenu)))))

(provide 'nvp-imenu)
;;; nvp-imenu.el ends here
