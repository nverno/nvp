;;; nvp-ruby.el --- rubls -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'ruby-mode)
(require 'inf-ruby nil t)
(nvp:decls
 :p ("ruby" "inf-ruby" "robe" "projectile-rails")
 :f (inf-ruby))

;;; Snippets

;; create arg initializtion from yas-text
;; (i,j) => @i = i\n@j = j or (io=$stdin, ...) => @io = io\n...
(defsubst nvp-ruby-yas-init-args ()
  (mapconcat (lambda (s) (concat "@" s " = " s))
             (--map (car (split-string it "=" t " "))
                    (split-string yas-text "[() ,]+" t " "))
             "\n"))

;;; Movement
(defun nvp-ruby-beginning-of-block ()
  (interactive)
  (ruby-end-of-block -1))

;;; Compile
(defun nvp-ruby-compile ()
  (interactive)
  (ruby-compilation-this-buffer)
  (other-window 1))

;;; Types
(defun nvp-ruby-yardocify-types (start end)
  "Fix types between START and END to conform to yardoc specs."
  (interactive
   (if (region-active-p) (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (save-mark-and-excursion
    (goto-char start)
    (while (re-search-forward "{\\([^[\n}]+\\)\\(\\[\\]\\)\?}" end t)
      (replace-match (if (match-string 2) "[Array<\\1>]" "[\\1]") t nil nil 0))))

;; -------------------------------------------------------------------
;;; REPL

(with-eval-after-load 'nvp-repl
  (nvp-repl-add '(ruby-mode ruby-ts-mode rspec-mode)
    :name 'ruby
    :modes '(inf-ruby-mode)
    :init #'nvp-ruby-start-robe
    :send-string #'ruby-send-string
    :send-region #'ruby-send-region
    :send-defun #'ruby-send-definition
    :send-sexp #'ruby-send-last-sexp
    :send-file #'ruby-load-file
    :send-buffer #'ruby-send-buffer
    :send-line #'ruby-send-line
    :history-file ".irb_history"
    :help-cmd '(:no-arg "help" :with-arg "? %s")
    :cd-cmd ".cd %s"
    :pwd-cmd ".pwd"))

;;; Robe
(defun nvp-ruby-start-robe (&optional _prefix)
  (interactive)
  (robe-mode)
  (save-window-excursion
    (or (ignore-errors (inf-ruby-console-auto))
        (inf-ruby inf-ruby-default-implementation)))
  (robe-start)
  (get-buffer-process inf-ruby-buffer))

;; add compilation jumps in traceback output from REPL
(defvar nvp-ruby-inf-compilation-regexp
  '("^\\s-+[0-9]+: from \\([^\(][^:]+\\):\\([0-9]+\\)" 1 2))

;; Interpret '?' in irb similar to octave/ess
(defconst nvp-ruby-inf-help
  '(("ruby" . "help\n%s\n")
    ("pry"  . "show-source -d %s")))

(defun nvp-ruby-inf-sender (proc str)
  (-if-let (help-str (and (string-match "^ *\\? *\\(.+\\)" str)
                          (match-string 1 str)))
      (progn
        (comint-simple-send
         proc (format
               (cdr (assoc inf-ruby-default-implementation nvp-ruby-inf-help))
               help-str)))
    (comint-simple-send proc str)))

;; -------------------------------------------------------------------
;;; Fold / Align

;; ruby-hacks.el
;; setup align for ruby-mode
(defconst align-ruby-modes '(ruby-mode ruby-ts-mode)
  "align-perl-modes is a variable defined in `align.el'.")

(defvar hs-special-modes-alist)
(with-eval-after-load 'hideshow
  (dolist (mode align-ruby-modes)
    (unless (assoc mode hs-special-modes-alist)
      (push (list mode "\\(def\\|do\\)" "end" "#") hs-special-modes-alist))))

(defconst ruby-align-rules-list
  '((ruby-comma-delimiter
     (regexp . ",\\(\\s-*\\)[^/ \t\n]")
     (modes  . align-ruby-modes)
     (repeat . t))
    (ruby-string-after-func
     (regexp . "^\\s-*[a-zA-Z0-9.:?_]+\\(\\s-+\\)['\"]\\w+['\"]")
     (modes  . align-ruby-modes)
     (repeat . t))
    (ruby-symbol-after-func
     (regexp . "^\\s-*[a-zA-Z0-9.:?_]+\\(\\s-+\\):\\w+")
     (modes  . align-ruby-modes)))
  "Alignment rules specific to the ruby mode.
See the variable `align-rules-list' for more details.")

(with-no-warnings
  (with-eval-after-load 'align
    (dolist (mode align-ruby-modes)
      (dolist (lst '(align-perl-modes
                     align-dq-string-modes
                     align-sq-string-modes
                     align-open-comment-modes))
        (add-to-list lst mode)))
    (dolist (it ruby-align-rules-list)
      (add-to-list 'align-rules-list it))))

;; -------------------------------------------------------------------
;;; Debug

(defun nvp-ruby--buffer-requires ()
  (save-excursion
    (goto-char (point-min))
    (let (res)
      (while
          (re-search-forward "^require +\\(?:['\"]\\)\\([^\n\"']+\\)" nil 'move)
        (push (match-string 1) res))
      res)))

(defun nvp-ruby-require (lib)
  (unless (cl-member lib (nvp-ruby--buffer-requires) :test 'string=)
    (save-excursion
      (goto-char (point-min))
      (forward-comment (point-max))
      (skip-syntax-backward " >")
      (insert (concat "\nrequire \"" lib "\"")))))

(defun nvp-ruby--insert-breakpoint ()
  (nvp-ruby-require "pry")
  (insert "binding.pry"))

(defun nvp-ruby--remove-breakpoints ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "\\(^require ['\"]pry['\"]\\|binding\.pry\\)\\s-*" nil 'move)
      (replace-match "")
      (delete-blank-lines))))

(defun nvp-ruby-breakpoint (arg)
  "Insert breakpoint, with prefix remove all pry stuff."
  (interactive "P")
  (if arg (nvp-ruby--remove-breakpoints)
    (nvp-ruby--insert-breakpoint)))

(provide 'nvp-ruby)
;;; nvp-ruby.el ends here
