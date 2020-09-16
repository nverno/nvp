;;; nvp-ruby.el --- rubls -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'ruby-mode)
(require 'inf-ruby)
(nvp-decls
 :f (ruby-compilation-this-buffer projectile-rails-root robe-mode robe-start))

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

;;; Robe
(defun nvp-ruby-start-robe ()
  (interactive)
  (robe-mode)
  (inf-ruby)
  (robe-start))

;;; Compile
(defun nvp-ruby-compile ()
  (interactive)
  (ruby-compilation-this-buffer)
  (other-window 1))

;;; REPL
;; add compilation jumps in traceback output from REPL
(defvar nvp-ruby-inf-compilation-regexp
  '("^\\s-+[0-9]+: from \\([^\(][^:]+\\):\\([0-9]+\\)" 1 2))

(defun nvp-ruby-switch-to-repl (eob-p)
  (interactive "P")
  (if (buffer-live-p (bound-and-true-p inf-ruby-buffer))
      (ruby-switch-to-inf eob-p)
    (nvp-ruby-start-robe)))

(defun nvp-ruby-send-block ()
  (interactive)
  (condition-case nil
      (progn
        (ruby-send-block-and-go)
        (ruby-switch-to-last-ruby-buffer))
    (error
     (progn
       (ruby-send-definition-and-go)
       (ruby-switch-to-last-ruby-buffer)))))

(defun nvp-ruby-send-last-sexp-and-step ()
  (interactive)
  (end-of-line)
  (ruby-send-last-sexp)
  (forward-char 1))

(defun nvp-ruby-send-file ()
  (interactive)
  (save-buffer)
  (ruby-send-region-and-go
   (point-min)
   (point-max)))

;;; Fold

(defvar hs-special-modes-alist)
(with-eval-after-load 'hideshow
  (unless (assoc 'ruby-mode hs-special-modes-alist)
    (push (list 'ruby-mode "\\(def\\|do\\)" "end" "#") hs-special-modes-alist)))

;;; Tidy / Align

;; ruby-hacks.el
;; setup align for ruby-mode
(defconst align-ruby-modes '(ruby-mode)
  "align-perl-modes is a variable defined in `align.el'.")

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
    (add-to-list 'align-perl-modes 'ruby-mode)
    (add-to-list 'align-dq-string-modes 'ruby-mode)
    (add-to-list 'align-sq-string-modes 'ruby-mode)
    (add-to-list 'align-open-comment-modes 'ruby-mode)
    (dolist (it ruby-align-rules-list)
      (add-to-list 'align-rules-list it))))

;; -------------------------------------------------------------------
;;; Debug

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

;; -------------------------------------------------------------------
;;; Rspec

(defconst nvp-rspec-font-lock-keywords
  `((,(regexp-opt '("expect" "describe" "it" "context" "before") 'symbols)
      (1 font-lock-function-name-face))))

(defun nvp-rspec-font-lock ()
  (font-lock-add-keywords 'ruby-mode nvp-rspec-font-lock-keywords))


(provide 'nvp-ruby)

;;; Local Variables:
;;; lisp-indent-function: common-lisp-indent-function
;;; End:

;;; nvp-ruby.el ends here
