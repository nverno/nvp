;;; nvp-perl.el --- Perl -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-parse))
(require 'cperl-mode)
(nvp:decls)


(with-eval-after-load 'nvp-repl
  (require 'nvp-perl-repl))

;;; Things at point
;; Skip back across `backchars' chars, then look for `forward-regexp',
;; returning cons of start and end of match.
(defsubst nvp-perl--looking-at (backchars &optional forward-regexp)
  (or forward-regexp (setq forward-regexp (format "[%s]+" backchars)))
  (save-excursion
    (skip-chars-backward backchars)
    (if (looking-at forward-regexp)
        (cons (point) (match-end 0))
      nil)))

(defun nvp-perl--module ()
  (nvp-perl--looking-at "_[:alpha:]:\\->" "[_[:alpha:]:]+"))
(put 'perl-module 'bounds-of-thing-at-point 'nvp-perl--module)

(defun nvp-perl--variable ()
  (nvp-perl--looking-at "[:alnum:]_$@#%*&=" "[[:alnum:]_$@#%*&]+"))
(put 'perl-variable 'bounds-of-thing-at-point 'nvp-perl--variable)


;; (("module" import1 import2 ...) ("module2" ... ))
(defun nvp-perl--parse-includes (&rest args)
  "List imports and used modules."
  (nvp-parse:buffer-file 'buffer nil args
    (save-excursion
      (goto-char (point-min))
      (let (res imports)
        (while (re-search-forward
                (nvp:concat
                 ;; Module
                 "\\<use\\>[ \t]*\\([.0-9A-Za-z:]+\\);?[ \t]*"
                 ;; Imports
                 "\\(?:qw(\\(?2:[^\)]+\\))\\|\\(?2:[^ \t]\\)\\)")
                nil 'move)
          (setq imports (match-string-no-properties 2))
          (push (cons (match-string-no-properties 1)
                      (if imports (split-string imports) nil))
                res))
        res))))

(nvp:defmethod nvp-parse-includes (&rest args)
  :modes (cperl-mode perl-mode perl-ts-mode)
  (nvp-perl--parse-includes args))

;;; Eldoc
(defun nvp-perl-eldoc-function ()
  (ignore-errors
    (nvp:unless-ppss 'soc
      (car (let ((cperl-message-electric-keyword nil))
             (cperl-get-help))))))

;;; Insert / Toggle
;; non-nil if point is in hash definition
(defsubst nvp-perl-hash-p ()
  (save-excursion
    (ignore-errors
      (backward-up-list)
      (beginning-of-line)
      (looking-at-p ".*%.*="))))

(defun nvp-perl-cycle (seq)
  (nvp-cycle (kbd "<tab>") seq :exit-fn #'nvp-perl-cycle-exit))

;; hook run after 'my' abbrev toggle is done
(defun nvp-perl-cycle-exit ()
  (unless (eq this-command 'keyboard-quit)
    (let ((char (char-before)))
      (pcase char
        (`?%
         (undo-boundary)
         (yas-expand-snippet
          "$1${2: = (\n  ${3:x} => $4\n$0);}" nil nil
          '((yas-indent-line 'auto))))))))

;; FIXME: inserts in weird places
;; Add a new perl use statement after the existing use statements.
(defun nvp-perl-add-use (&optional module)
  (interactive
   (list
    (read-from-minibuffer "Module: " (thing-at-point 'perl-module))))
  (if (not (assoc-string module (nvp-parse-includes)))
      (save-excursion
        (goto-char (point-min))
        ;; goto end of last use declaration
        (while (re-search-forward "\\<use\\>" nil t))
        ;; if still at beginning of buffer, skip over comments
        (when (bobp)
          (forward-comment (point-max)))
        (forward-line 1)
        (insert (format "use %s%s\n" module
                        (if (string-match-p ";" module) "" ";"))))))


;; ------------------------------------------------------------
;;; Debug

;; Launch debugger
;; on windows just run in an external shell
(defun nvp-perl-debug (_arg)
  (interactive "P")
  (nvp:with-gnu/w32
      (let ((process-environment
             (cons "PERL5DB_THREADED=1" process-environment)))
        (call-interactively 'cperl-db))
    (perl-w32tools-debug-shell _arg)))

;; Insert 'use Data::Printer; p `var'' where `var' is the variable
;; near the point.  If invoked with an argument, comments out the
;; line where `var' is found.
(defun nvp-perl-insert-debug-statement (var &optional comment)
  (interactive
   (let ((var (thing-at-point 'perl-variable t)))
     (list (read-string (nvp:prompt-default "Expression to dump: " var) nil nil var)
           current-prefix-arg)))
  (when comment
    (progn
      (beginning-of-line)
      (cperl-indent-command)
      (insert "# ")))
  (save-excursion
    (unless
        (re-search-backward
         "^[[:space:]]*[^#]*[[:space:]]*use[[:space:]]+Data::Printer;" nil t)
      (goto-char (point-min))
      (nvp-perl-add-use "Data::Printer")))
  (save-excursion
    (back-to-indentation)
    (split-line)
    (insert (format "p %s;" var))
    (comment-indent)
    (insert "DEBUG")))

(provide 'nvp-perl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-perl.el ends here
