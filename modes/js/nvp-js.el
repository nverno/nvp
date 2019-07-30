;;; nvp-js.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (defvar nodejs-repl-process-name))
(declare-function nodejs-repl-send-region "nodejs")
(declare-function nodejs-repl-send-last-sexp "nodejs")
(declare-function nodejs-repl-switch-to-repl "nodejs-repl")
(declare-function nodejs-repl "nodejs-repl")

(nvp-package-define-root :snippets t)

;; -------------------------------------------------------------------
;;; Utils

;; syntax helpers

(defun nvp-js-method-p ()
  (save-excursion
    (word-search-backward "function")
    (looking-back ": " (line-beginning-position))))

(defun nvp-js-function-declaration-p ()
  (save-excursion
    (word-search-backward "function")
    (looking-back "^\\s *" (line-beginning-position))))

(defun nvp-js-snippet-punctuation ()
  (if (nvp-js-method-p)
      (when (not (looking-at "[ \n\t\r]*[},]"))
        (insert ","))
    (unless (nvp-js-function-declaration-p)
      (if (looking-at "$") (insert ";")))))

(defun nvp-js-snippet-function-name ()
  (if (nvp-js-function-declaration-p) "name" ""))

;; (defvar keyword-function
;;   '(("\\(function\\)\\>" (0 (prog1 ()
;;                               (compose-region (match-beginning 1)
;;                                               (match-end 1)
;;                                               "\u0192"))))))
;; (font-lock-add-keywords 'js2-mode keyword-function)

;; -------------------------------------------------------------------
;;; Commands

(nvp-newline nvp-js-newline-dwim nil :pairs (("{" "}")))

;; -------------------------------------------------------------------
;;; REPL

;; track working buffer when switching to node buffer
(defvar nodejs-working-buffer)

;; (defadvice nodejs-repl (before set-nodejs-working-buffer activate)
;;   (setq nodejs-working-buffer (current-buffer))
;;   ad-do-it)

;; (defadvice nodejs-repl-switch-to-repl
;;     (before set-nodejs-working-buffer activate)
;;   (setq nodejs-working-buffer (current-buffer))
;;   ad-do-it)

;; switch b/w repl and working buffer
(defun nvp-js-nodejs-switch-to-repl ()
  (interactive)
  (if (not (boundp 'nodejs-repl-process-name))
      (nodejs-repl)
    (if (eq (get-process nodejs-repl-process-name)
            (get-buffer-process (current-buffer)))
        (and (buffer-live-p nodejs-working-buffer)
             (switch-to-buffer-other-window
              nodejs-working-buffer))
      (nodejs-repl-switch-to-repl))))

(defun nvp-js-nodejs-region-or-sexp ()
  (interactive)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'nodejs-repl-send-region)
      (call-interactively 'nodejs-repl-send-last-sexp))
  (forward-line))

;; -------------------------------------------------------------------
;;; Help

(declare-function js2-display-error-list "js2-mode")
(declare-function tern-get-docs "tern")

(defun nvp-js-help-at-point ()
  (interactive)
  (cond
   ((member 'js2-echo-error (get-text-property (point) 'cursor-sensor-functions))
    (js2-display-error-list))
   (t (tern-get-docs))))

(provide 'nvp-js)
;;; nvp-js.el ends here
