;;; nvp-ruby-repl.el --- Ruby REPL -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-repl)
(require 'comint)
(require 'inf-ruby nil t)
(nvp:decls :p (ruby inf-ruby robe) :f (robe-mode inf-ruby))


;; add compilation jumps in traceback output from REPL
(defvar nvp-ruby-inf-compilation-regexp
  '("^\\s-+[0-9]+: from \\([^\(][^:]+\\):\\([0-9]+\\)" 1 2))

;; Interpret '?' in irb similar to octave/ess
(defconst nvp-ruby-inf-help
  '(("ruby" . "help\n%s\n")
    ("pry"  . "show-source -d %s")
    (nil    .  "")))

(defvar-local nvp-ruby-inf-command "ruby")

(defun nvp-ruby-repl--cmd-handlers ()
  (list (cons "?" (assoc-default nvp-ruby-inf-command nvp-ruby-inf-help))))

(defun nvp-ruby-repl-init (&optional _prefix)
  (when (fboundp 'robe-mode)
    (unless robe-mode (robe-mode 1)))
  (save-window-excursion
    (or (ignore-errors (inf-ruby-console-auto))
        (inf-ruby inf-ruby-default-implementation)))
  (and (fboundp 'robe-start) (robe-start))
  (with-current-buffer inf-ruby-buffer
    (setq-local nvp-ruby-inf-command (string-trim
                                      (string-remove-prefix
                                       "bundle exec" inf-ruby-buffer-command)))
    (get-buffer-process (current-buffer))))

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
    :help-cmd '(:no-arg "help\nnil\n" :with-arg "? %s")
    :cd-cmd ".cd %s"
    :pwd-cmd ".pwd"
    :cmd-handlers #'nvp-ruby-repl--cmd-handlers))

(provide 'nvp-ruby-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-ruby-repl.el ends here
