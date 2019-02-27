;;; nvp-he-elisp.el --- elisp hippie expansion -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/elisp-utils
;; Last modified: <2019-02-20 14:55:58>
;; Package-Requires: 
;; Created:  6 May 2017

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(require 'hippie-exp)
(autoload 'string-prefix-p "subr-x")
(autoload 'nvp-he-try-expand-flex-lisp "nvp-he-lisp")

;;; Variables
(defvar-local nvp-he-elisp--prefixes nil)

;; convert prefixes:
;; - if 'all, replace entirety
;; - if 'regex, just replace matching prefix
(defvar nvp-he-elisp--replacements
  '(("subr-x"        remove)
    ("cl-lib"        all   "cl"  )
    ("nvp-macro"     all   "nvp" )
    ("hippie-expand" regex "he"  )
    ("hippie-exp"    regex "he"  )))

;; replace some libraries with different prefixes
(defun nvp-he-elisp--replace-namespace (lst)
  (let (res)
    (dolist (item lst res)
      (if (not (catch 'modified
                 (dolist (val nvp-he-elisp--replacements)
                   (when (string-prefix-p (car val) item)
                     (if (pcase (cadr val)
                           ('all (push (caddr val) res) t)         ;replace entirely
                           ('regex (push (replace-regexp-in-string ;replace match
                                          (car val) (caddr val) item)
                                         res)
                                   t)
                           ('remove t)                             ;ignore it
                           (_ nil))                                ;not done yet
                         (throw 'modified t))))))
          (push item res)))))                                      ;leave as is

;; Pull out a possible prefix for namespaces mentioned in file
;; i.e by either 'require or 'provide
(defun nvp-he-elisp--get-namespaces ()
  (save-excursion
    (goto-char (point-min))
    (let ((vars '("nvp")))
      (while (search-forward-regexp
              (nvp-concat "\\s-*(\\(?:require\\|provide\\)[ \t]*\'"
                          "\\([-:_.a-zA-Z0-9]+\\)")
              nil t)
        (push (buffer-substring-no-properties (match-beginning 1) (match-end 1))
              vars))
      (setq nvp-he-elisp--prefixes
            (cl-remove-duplicates        ;replace some library prefixes
             (nvp-he-elisp--replace-namespace vars) :test 'string=)))))

;; return the local namespaces
(defun nvp-he-elisp--namespaces ()
  (or (bound-and-true-p nvp-he-elisp--prefixes)
      (setq nvp-he-elisp--prefixes (nvp-he-elisp--get-namespaces))))

(defun nvp-he-try-elisp-with-local-namespace (old)
  "Try to complete as emacs lisp symbol by adding local namespace prefix.
The current search string must be prefixed with '-', or returns nil and 
onto the next one."
  (catch 'quit
    (unless old
      (he-init-string (he-lisp-symbol-beg) (point))
      (if (not (string-prefix-p "-" he-search-string))
          (throw 'quit nil)
        (if (not (he-string-member he-search-string he-tried-table))
            (setq he-tried-table (cons he-search-string he-tried-table)))
        (setq he-expand-list             ;build expansion list
              (and (not (equal "" he-search-string))
                   (mapcar (lambda (ns)
                             (concat ns he-search-string))
                           (nvp-he-elisp--namespaces))))))
    (while (and he-expand-list            ;remove candidates already found
                (he-string-member (car he-expand-list) he-tried-table))
      (setq he-expand-list (cdr he-expand-list)))
    (prog1 (not (null he-expand-list))    ;return t if expansion is found
      (if (not he-expand-list)
          (and old (he-reset-string))
        (he-substitute-string (pop he-expand-list))))))

;; Hippie expand for elisp namespaced expansions.
;; This version doesn't need a '-' prefix, but instead tries all the symbols
;; found in the local namespaces. Thus, it does a lot more work, so is
;; tried last.
(defun nvp-he-try-elisp-symbol-sans-namespace (old)
  (let ((expansion)
        (prefs (nvp-he-elisp--namespaces)))
    (unless old
      (he-init-string (he-lisp-symbol-beg) (point))
      (if (not (string= he-search-string ""))
          (while (and prefs (not expansion))
            (setq expansion
                  (try-completion
                   (concat (pop prefs) "-" he-search-string)
                   obarray
                   (function (lambda (sym)
                               (or (boundp sym)
                                   (fboundp sym)
                                   (symbol-plist sym))))))
            (if (or (eq expansion t)
                    (string= expansion he-search-string)
                    (he-string-member expansion he-tried-table))
                (setq expansion ()))))
      (if (not expansion)
          (and (he-reset-string) nil)
        (and (he-substitute-string expansion) t)))))

;;;###autoload
(defun nvp-he-elisp-setup ()
  "Setup hippie expand functions for source buffers."
  (make-local-variable 'hippie-expand-try-functions-list)
  (setq hippie-expand-try-functions-list
        (cons 'nvp-he-try-elisp-with-local-namespace    ;try '-' prefixed first
              hippie-expand-try-functions-list)) ;defaults setup in init
  (add-to-list 'hippie-expand-try-functions-list #'nvp-he-try-expand-flex-lisp t)
  (add-to-list 'hippie-expand-try-functions-list ;using add-to-list since
               #'try-complete-lisp-symbol t)     ;some may already be in there
  (add-to-list 'hippie-expand-try-functions-list
               #'try-complete-lisp-symbol-partially t)
  (add-to-list 'hippie-expand-try-functions-list 
  	       #'nvp-he-try-elisp-symbol-sans-namespace t))

(provide 'nvp-he-elisp)
;;; nvp-he-elisp.el ends here