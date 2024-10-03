;;; nvp-repl-menu.el --- Repl transient menu -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls :f (nvp-comint-menu))

(require 'nvp-repl)
(require 'transient)

(nvp:transient-toggle nvp-repl-config-menu
  nvp-repl-load-startup-file
  nvp-repl-dedicated-window)

(transient-define-infix nvp-repl--set-display ()
  "Set display action."
  :description "Set display action"
  :class 'transient-lisp-variable
  :variable 'nvp-repl-display-action
  :reader
  (lambda (prompt initial-input history)
    (intern
     (completing-read
      prompt (mapcar #'car nvp-repl--display-actions) nil t initial-input history))))

(autoload 'nvp-dev-describe-variable "nvp-dev")

(defun nvp-repl-describe-repl ()
  "Pretty print current repl struct."
  (interactive)
  (let ((repl (nvp-repl-current)))
    (unless repl
      (user-error "No current repl"))
    (with-current-buffer (nvp-dev-describe-variable 'nvp-repl--repl-cache)
      (re-search-forward (format "^\\s-*%S" (nvp--repl-name repl)) nil t)
      (scroll-other-window (line-number-at-pos)))))

;;;###autoload(autoload 'nvp-repl-menu "nvp-repl-menu" nil t)
(transient-define-prefix nvp-repl-menu ()
  "REPL menu"
  [[ :if-non-nil nvp-repl--current
     "Send"
     ("s" "Last sexp" nvp-repl-send-sexp)
     ("S" "Statement or sentence" nvp-repl-send-stmt-or-sentence)
     ("l" "Line or region" nvp-repl-send-line)
     ("r" "Region" nvp-repl-send-region)
     ("f" "Defun" nvp-repl-send-defun)
     ("d" "Defun or region" nvp-repl-send-defun-or-region)
     ("b" "Buffer" nvp-repl-send-buffer)
     ("F" "Load File" nvp-repl-send-file)]
   [ :if-non-nil nvp-repl--current
     "Eval"
     ("E" "String" nvp-repl-eval-string)
     ("e" "Last sexp" nvp-repl-eval-sexp)]
   [ :if nvp-repl-current
     "Commands"
     ("k" "Clear" nvp-repl-clear :transient t)
     ("h" "Help" nvp-repl-help)
     ("c" "Config" nvp-repl-config)
     ("w" "Show working directory/buffer" nvp-repl-pwd :transient t)
     ("W" "Change Working directory/buffer" nvp-repl-cd)]]
  [["Repl"
    ("j" "Jump" nvp-repl-jump)
    ("q" "Interrupt/kill process" nvp-repl-interrupt-or-kill-process
     :if nvp-repl-current)
    ("C" "Comint menu" nvp-comint-menu :if-derived comint-mode)]
   ["Manage Repls"
    ("M-?" "Describe repl" nvp-repl-describe-repl :if nvp-repl-current)
    (":r" "Remove repl" nvp-repl-remove)]
   ["Settings"
    (":s" nvp-repl--set-display)
    (":d" nvp-repl-config-menu--toggle-nvp-repl-dedicated-window)
    (":l" "Load startup file"
     nvp-repl-config-menu--toggle-nvp-repl-load-startup-file)]])

(provide 'nvp-repl-menu)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-repl-menu.el ends here
