;;; nvp-elisp.el --- elisp helpers  -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Last modified: <2019-02-23 18:26:42>
;; URL: https://github.com/nverno/elisp-utils
;; Package-Requires: 
;; Created: 31 October 2016

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

;;; FIXME:
;; - update / remove macroify
;; - update provides?
;; - merge with nvp
;; - imenu filter out package related headers

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(declare-function idomenu "idomenu")
(declare-function paredit-mode "paredit")
(declare-function nvp-toggle-local-variable "nvp-toggle")
(nvp-declare "company-elisp" company-elisp--candidates-predicate
  company-elisp--fns-regexp company-grab-symbol)

(nvp-package-define-root :snippets t)

;; -------------------------------------------------------------------
;;; Util

;; modified from company-elisp to incorporate more things
;; used to determine abbrev expansion / toggling
(eval-when-compile (require 'company-elisp))
(let-when-compile
    ((el-defs '("defun" "defmacro" "defsubst" "defmethod" "defclass" "defgeneric"
                "define-advice" "defadvice" "add-advice" "add-function"
                ))
     (el-vdefs '("let" "cond" "lexical-let" "if-let" "when-let" "lambda"
                 "labels" "flet"
                 "pcase" "pcase-dolist" "pcase-lambda"
                 "pcase-exhaustive" "pcase-let")))
  (let ((vars-re
         (eval-when-compile
           (apply #'company-elisp--fns-regexp (append el-defs el-vdefs))))
        (defs-re
          (eval-when-compile
            (concat "([ \t\n]*" (apply #'company-elisp--fns-regexp el-defs)))))
    ;; include generics
    (defvar nvp-elisp-defuns-regexp defs-re)
    ;; additional let macros, pcase, cond, etc.
    (defvar nvp-elisp-var-binding-regexp vars-re)))

;; -------------------------------------------------------------------
;;; FIXME: functions to fix or remove
;; remove this? can be found from `load-history'
(defun nvp-elisp-provide-name ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           "\\<provide\\>[ \t']+\\([-0-9A-Za-z]+\\)" nil t)
      (match-string-no-properties 1))))

;;; FIXME: update or remove
;; convert selected bindings to macro form and align
(defun nvp-macroify-bindings (start end)
  (interactive "r")
  (goto-char start)
  (let ((map (save-excursion
               (when (re-search-forward "\\([a-zA-Z0-9-]+\\)-map"
                                        end t)
                 (match-string-no-properties 1)))))
    (when map
      (let (binds)
        (while (re-search-forward
                "\\(\"[^\"]+\"\\))?[\n\t ]*[#']*\\([a-zA-Z0-9-]+\\)"
                end t)
          (push (format "(%s . %s)"
                        (match-string-no-properties 1)
                        (match-string-no-properties 2))
                binds))
        (goto-char start)
        (insert (concat "(nvp-bindings \"" map "\" nil \n  "
                        (mapconcat 'identity (nreverse binds) "\n  ")
                        ")\n"))
        (goto-char start)
        (mark-sexp)
        (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)\\. ")))))

;; ------------------------------------------------------------
;;; Eval

;; Eval region from BEG to END if active, otherwise the last sexp.
(defun nvp-elisp-eval-last-sexp-or-region (arg)
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (let ((print-length (window-total-height))
          (print-level))
      (if arg (pp-eval-expression (pp-last-sexp))
        (save-excursion
          (pp-eval-expression (pp-last-sexp))))
      (with-current-buffer "*Pp Eval Output*"
        (display-buffer (current-buffer) t)))))

;; Jump to end of line and try eval if not looking back at `)'.
(defun nvp-elisp-eval-last-sexp-or-eol (arg)
  (interactive "P")
  (if (not (looking-back "\\s-*)" (line-beginning-position)))
      (end-of-line))
  (nvp-elisp-eval-last-sexp-or-region nil)
  (and arg (forward-sexp)))

(defun nvp-elisp-eval-and-replace ()
  "Replace preceding sexp with its evaluated value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
	     (current-buffer))
    (error (message "invalid expression")
	   (insert (current-kill 0)))))

;; -------------------------------------------------------------------
;;; Indentation

;; cl-flet etc. indentation
(with-eval-after-load 'cl-indent
  (let ((indent (get 'flet 'common-lisp-indent-function)))
    (mapc #'(lambda (f) (put f 'common-lisp-indent-function indent))
          '(cl-flet cl-flet* labels cl-labels cl-macrolet))))
(put 'cl-loop 'lisp-indent-function 'common-lisp-indent-function)
(put 'cl-labels 'lisp-indent-function 'common-lisp-indent-function)
(put 'lisp-indent-function 'safe-local-variable 'symbolp)

;; -------------------------------------------------------------------
;;; Insert / Toggle 

;; indent region with common-lisp-indent function
;; with prefix toggle b/w lisp-indent and common-lisp-indent
(defun nvp-elisp-toggle-cl-indent (&optional start end switch)
  "Indent region with `common-lisp-indent-function'.
With \\[universal-argument] toggle buffer-local `common-lisp-indent-function'."
  (interactive "r\nP")
  (when switch
    (setq-local lisp-indent-function
                (if (eq lisp-indent-function 'lisp-indent-function)
                    'common-lisp-indent-function
                  'lisp-indent-function)))
  (let ((lisp-indent-function 'common-lisp-indent-function))
    (put 'cl-flet 'common-lisp-indent-function
         (get 'flet 'common-lisp-indent-function))
    (indent-region start end)))

(defun nvp-elisp-toggle-lexical ()
  "Toggle `lexical-binding' on/off."
  (interactive)
  (require 'nvp-toggle)
  (nvp-toggle-local-variable 'lexical-binding t))

;;;###autoload
(defun nvp-elisp-toggle-auto ()
  "Toggle autoload on/off when in or directly before sexp.
If in function definition, toggle autoload cookie.
If in `declare-function', convert to autoload."
  (interactive)
  (save-match-data
    (save-excursion
      (ignore-errors (goto-char (car (nth 9 (syntax-ppss)))))
      (cond
       ((looking-at "(declare-function\\s-+")
        (replace-match "(autoload '"))
       ((looking-at "(autoload\\s-+'")
        (replace-match "(declare-function "))
       ((looking-at nvp-elisp-defuns-regexp)
        (and (bobp) (insert "\n"))
        (forward-line -1)
        (if (looking-at ";;;###autoload[ \t]*\n")
            (replace-match "")
          (unless (looking-at-p "^\\s-*$")
            (end-of-line))
          (insert "\n;;;###autoload")))
       (t (message "Location not autoloadable"))))))

;; ------------------------------------------------------------
;;; Abbrevs: expansion predicates

;; don't expand when prefixed by '-'
(defvar nvp-lisp-abbrev-re "\\(\\_<[_:\\.A-Za-z0-9/=<>]+\\)")

;; Don't expand in strings, comments, function args, let bindings or after '-/'
;; Note: Could also account for dolist/dotimes/etc
(eval-when-compile
  (nvp-declare "" nvp-abbrev-grab)
  (defvar company-elisp-var-binding-regexp)
  (defmacro nvp-elisp-abbrev--expand-p (type)
    (declare (debug t))
    `(and (or (memq this-command '(expand-abbrev nvp-abbrev-expand-after-symbols))
              (not (memq last-input-event '(?/ ?- ?= ?> ?<))))
          (not (or (let ((ppss (syntax-ppss)))
                     (or (elt ppss 3) (elt ppss 4)))
                   (let ((company-elisp-var-binding-regexp
                          nvp-elisp-var-binding-regexp))
                     (eq (company-elisp--candidates-predicate (nvp-abbrev-grab))
                         ,type)))))))

;; function expansion
(defun nvp-elisp-abbrev-expand-fn-p ()
  (nvp-elisp-abbrev--expand-p 'boundp))

;; variable expansion
(defun nvp-elisp-abbrev-expand-var-p ()
  (nvp-elisp-abbrev--expand-p 'fboundp))

;; ------------------------------------------------------------
;;; REPL / IELM
(declare-function ielm "ielm")

(nvp-repl-switch "ielm" (:repl-mode 'inferior-emacs-lisp-mode
                         :repl-find-fn #'(lambda () (get-buffer "*ielm*"))
                         :repl-wait 0.1
                         :repl-switch-fn 'switch-to-buffer-other-window)
  (with-current-buffer (get-buffer-create "*ielm*")
    (cl-letf (((symbol-function 'pop-to-buffer-same-window) #'ignore))
      (ielm))
    (current-buffer)))

(with-eval-after-load 'ielm
  ;; synchronize with default switching function and update default-directory
  (with-no-warnings
    (define-advice ielm-change-working-buffer (:after (&rest _args) "update-src")
      (process-put (ielm-process) :src-buffer ielm-working-buffer)
      (setq default-directory
            (buffer-local-value 'default-directory ielm-working-buffer)))))

;; ------------------------------------------------------------
;;; Imenu

;; TODO: filter out package fluff
(eval-and-compile
  (nvp-defvar nvp-elisp-imenu-comment-headers
   (let* ((prefix "^;;\\(?:;\\|[*]+\\| |\\)\\s-*")
          (hdr (concat prefix "\\([^#].*\\)\\s-*$"))
          (_pkg-hdrs
           (concat prefix "\\("
                   (regexp-opt '("Commentary:" "Code:" "Documentation:"
                                 "History:" "ChangeLog:" "Change Log:"))
                   "\\|.*\\.el\\)")))
     `((nil ,hdr 1))))

 (nvp-defvar nvp-elisp-imenu-comment-headers-1
   `(("Headers" ,(cadar nvp-elisp-imenu-comment-headers) 1)
     ("Libs" "^;;\\s-*[*]\\s-*\\(?:[Ll]ibs?\\):\\s-*\\([[:alnum:]- /]+\\)" 1)))

 (defvar nvp-elisp-imenu-comment-headers-2
   '(("Sub-Headers" "^;;---*\\s-*\\([^-\n]+\\)\\s-*-*$" 1))))

(provide 'nvp-elisp)
;;; nvp-elisp.el ends here
