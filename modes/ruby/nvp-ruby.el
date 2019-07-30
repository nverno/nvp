;;; nvp-ruby.el --- rubls -*- lexical-binding: t; -*-
;;; Commentary:
;; FIXME:
;; - use nvp-yas funcs
;; - 
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (defvar projectile-tags-file-name))
(require 'ruby-mode)
(require 'inf-ruby nil t)
(require 'robe nil t)

(nvp-decl :pre "ruby" switch-to-inf send-block-and-go switch-to-last-ruby-buffer
          send-definition-and-go send-last-sexp send-region-and-go)
(nvp-decl inf-ruby ruby-compilation-this-buffer projectile-rails-root
          robe-mode robe-start)
(nvp-decls)

;; (defun my-ruby-smart-return ()
;;   (interactive)
;;   (when (memq (char-after) '(?\| ?\" ?\'))
;;     (forward-char))
;;   (call-interactively 'newline-and-indent))

;; -------------------------------------------------------------------
;;; Utils

(defsubst nvp-ruby-version ()
  (let ((str (shell-command-to-string "ruby --version")))
    (and (string-match "\\([0-9.]+\\)" str)
         (match-string-no-properties 1 str))))

;; convert file[_-.]name to FileName
(defsubst nvp-ruby-camelize (str &optional seps)
  (mapconcat 'identity
             (mapcar 'capitalize (split-string str (or seps "[._-]"))) ""))

;; gather required libs in file
(defun nvp-ruby-requires ()
  (save-excursion
    (goto-char (point-min))
    (let (res)
      (while (re-search-forward
              "^require +\\(?:['\"]\\)\\([^\n\"']+\\)" nil 'move)
        (push (match-string 1) res))
      res)))

;; insert require
(defun nvp-ruby-insert-require (lib)
  (save-excursion
    (goto-char (point-min))
    (forward-comment (point-max))
    (skip-syntax-backward " >")
    (insert (concat "\nrequire '" lib "'"))))

(defsubst nvp-ruby-require (lib)
  (unless (cl-member lib (nvp-ruby-requires) :test 'string=)
    (nvp-ruby-insert-require lib)))

;; ------------------------------------------------------------
;;; Commands

(defun nvp-ruby-start-robe ()
  (interactive)
  (robe-mode)
  (inf-ruby)
  (robe-start))

;; Compile
(defun nvp-ruby-compile ()
  (interactive)
  (ruby-compilation-this-buffer)
  (other-window 1))

;; Movement
(defun nvp-ruby-beginning-of-block ()
  (interactive)
  (ruby-end-of-block -1))

(defun nvp-ruby-mark-block ()
  (interactive)
  (nvp--mark-defun))

;; code-folding
(eval-when-compile
  (require 'hideshow)
  (defmacro fold-show (&optional all)
    `(if (hs-already-hidden-p)
         (if ,all (hs-show-all) (hs-show-block))
       (if ,all (hs-hide-all) (hs-hide-block)))))

;; Fold classes/defs or just defs with prefix.
(defun nvp-ruby-fold-class/def (arg)
  (interactive "P")
  (nvp-with-hs-block "\\(?:def\\|class\\)" "\\(?:end\\)"
    (fold-show arg)))

(defun nvp-ruby-fold (arg)
  (interactive "P")
  (nvp-with-hs-block "\\(?:def\\)" "\\(?:end\\)"
    (fold-show arg)))

;; -------------------------------------------------------------------
;;; REPL

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

;; -------------------------------------------------------------------
;;; Debug

;; insert breakpoint
(defun nvp-ruby-insert-breakpoint ()
  (nvp-ruby-require "pry")
  (insert "binding.pry"))

;; remove pry stuff
(defun nvp-ruby-remove-breakpoints ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "\\(^require ['\"]pry['\"]\\|binding\.pry\\)\\s-*" nil 'move)
      (replace-match "")
      (delete-blank-lines))))

;; insert breakpoint, with prefix remove all pry stuff
(defun nvp-ruby-breakpoint (arg)
  (interactive "P")
  (if arg (nvp-ruby-remove-breakpoints)
    (nvp-ruby-insert-breakpoint)))

;; ------------------------------------------------------------
;;; Tags

(defun nvp-ruby-update-rails-ctags ()
  "Update ctags for rails project."
  (interactive)
  (let ((default-directory
         (or (projectile-rails-root) default-directory)))
    (shell-command
     (concat "ctags -a -e -f "
             projectile-tags-file-name
             " --tag-relative -R app lib vender test"))))

;; -------------------------------------------------------------------
;;; Snippets

;; create arg initializtion from yas-text
;; (i,j) => @i = i\n@j = j
(defsubst nvp-ruby-yas-init-args ()
  (mapconcat (lambda (s) (concat "@" s " = " s))
             (split-string yas-text "[() ,]+" t " ")
             "\n"))

(defun nvp-ruby-yas-camelize-bfn ()
  (nvp-ruby-camelize (nvp-path 'bfse)))

(provide 'nvp-ruby)

;;; Local Variables:
;;; lisp-indent-function: common-lisp-indent-function
;;; End:

;;; nvp-ruby.el ends here
