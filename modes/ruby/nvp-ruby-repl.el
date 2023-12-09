;;; nvp-ruby-repl.el --- Ruby REPL -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-repl)
(require 'comint)
(require 'inf-ruby nil t)
(nvp:decls :p (inf-ruby robe))

;; add compilation jumps in traceback output from REPL
(defvar nvp-ruby-inf-compilation-regexp
  '("^\\s-+[0-9]+: from \\([^\(][^:]+\\):\\([0-9]+\\)" 1 2))

;; Interpret '?' in irb similar to octave/ess
(defconst nvp-ruby-inf-help
  '(("ruby" . "help\n%s\n")
    ("pry"  . "show-source -d %s")))

;; translate '?' in STR
(defun nvp-ruby--repl-input-filter (str)
  (-if-let (help-str (and (string-match "^ *\\? *\\(.+\\)" str)
                          (match-string 1 str)))
      (format (cdr (assoc inf-ruby-default-implementation nvp-ruby-inf-help))
              help-str)
    str))

;; For `comint-input-sender', to handle special repl commands
(defun nvp-ruby-inf-sender (proc str)
  (comint-simple-send proc (nvp-ruby--repl-input-filter str)))

(defun nvp-ruby-repl-init (&optional _prefix)
  (when (fboundp 'robe-mode)
    (unless robe-mode (robe-mode 1)))
  (save-window-excursion
    (or (ignore-errors (inf-ruby-console-auto))
        (inf-ruby inf-ruby-default-implementation)))
  (and (fboundp 'robe-start) (robe-start))
  (get-buffer-process inf-ruby-buffer))

(when (fboundp 'inf-ruby-mode)
  (nvp-repl-add '(ruby-mode ruby-ts-mode rspec-mode)
    :name 'ruby
    :modes '(inf-ruby-mode)
    :init #'nvp-ruby-repl-init
    :send-string #'ruby-send-string
    :send-region #'ruby-send-region
    :send-defun #'ruby-send-definition
    :send-sexp #'ruby-send-last-sexp
    :send-statement #'ruby-send-last-stmt
    :send-file #'ruby-load-file
    :send-buffer #'ruby-send-buffer
    :send-line #'ruby-send-line
    :eval-sexp #'ruby-send-last-sexp
    :history-file ".irb_history"
    :help-cmd '(:no-arg "help" :with-arg "? %s")
    :cd-cmd ".cd %s"
    :pwd-cmd ".pwd"))

(provide 'nvp-ruby-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-ruby-repl.el ends here
