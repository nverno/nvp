;;; nvp-ielm.el --- ielm/lisp-interaction -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; FIXME:
;; - multi-line ielm history ==> how to change and use `comint-input-ring-separator'
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'lisp-mode)
(require 'ielm)
(nvp:decls :p (nvp-repl)
           :f (nvp-repl-current-buffer) :v (nvp-repl--process-buffers))

(defvar nvp-ielm-hippie-expanders
  '(nvp-he-try-expand-history
    nvp-try-expand-flex-or-dabbrev
    nvp-try-expand-flex-or-dabbrev-other-buffers
    ;; nvp-try-expand-dabbrev-closest-first
    ;; try-expand-dabbrev-all-buffers
    try-expand-dabbrev-from-kill
    try-complete-lisp-symbol
    try-complete-lisp-symbol-partially))

;; use `pop-to-buffer' and set local `ielm-working-buffer'
(define-advice ielm (:around (orig-fn &rest _args) "pop-to-buffer")
  (let ((orig-buff (current-buffer)))
    (with-current-buffer (get-buffer-create "*ielm*")
      (nvp:with-letf #'pop-to-buffer-same-window #'ignore
        (funcall orig-fn))
      (prog1 (current-buffer)
        (setq-local ielm-working-buffer orig-buff)
        (pop-to-buffer (current-buffer))))))

;; ielm-return always wants to eval when smartparens close sexps
(defun nvp-ielm-nl (&optional arg)
  "Insert nl and indent unless point is at end-of-line."
  (interactive "P")
  (if (eolp)
      (ielm-return arg)
    (let (ielm-dynamic-return)
      (newline-and-indent))))

;; Works as both command for `nvp-repl-help' and handler for "," in repl
(defun nvp-ielm-help (&optional arg _proc)
  (interactive "P")
  (and arg (setq arg (nvp:as-symbol arg)))
  (save-window-excursion
    (funcall-interactively
     (cond ((null arg)
            (setq arg ielm-working-buffer)
            #'describe-mode)
           ((fboundp arg) #'describe-function)
           ((boundp arg) #'describe-variable)
           (t (user-error "unbound symbol: '%S'" arg)))
     arg))
  (prog1 t (display-buffer (help-buffer))))

(defun nvp-ielm-cd (&optional arg)
  (interactive "P")
  (if (derived-mode-p 'inferior-emacs-lisp-mode)
      (--if-let (gethash (current-buffer) nvp-repl--process-buffers)
          (and (buffer-live-p it)
               (ielm-change-working-buffer it))
        (user-error "no source buffer for '%S'" (current-buffer)))
    (ielm-change-working-buffer (current-buffer)))
  (format "(setq default-directory \"%s\")" arg))

(defun nvp-ielm-pwd ()
  (interactive)
  (ielm-print-working-buffer)
  "ielm-working-buffer")

(defun nvp-ielm-send-string (_proc str &optional for-effect)
  "Send STR to ielm without inserting into repl."
  (with-current-buffer (nvp-repl-current-buffer)
    (ielm-eval-input str for-effect)))

(with-eval-after-load 'nvp-repl
  (nvp-repl-add
    '(emacs-lisp-mode lisp-data-mode lisp-interaction-mode command-history-mode)
    :name 'ielm
    :modes '(inferior-emacs-lisp-mode)
    :init #'ielm
    :init-use-hook t
    :procname "ielm"
    :eval-output-filter (lambda (s) (replace-regexp-in-string "[ \n\t]+" " " s))
    :send-input #'ielm-send-input
    :send-string #'nvp-ielm-send-string
    :cmd-handlers '(("," . nvp-ielm-help)) ; leave "?" for characters
    :help-cmd #'nvp-ielm-help
    :pwd-cmd #'nvp-ielm-pwd
    :cd-cmd #'nvp-ielm-cd))

;;; Fonts
;; (defconst nvp-ielm-font-lock-keywords
;;   `(,@(mapcar #'nvp-comint-font-lock-keywords lisp-el-font-lock-keywords-2)
;;     ,@(mapcar #'nvp-comint-font-lock-keywords lisp-cl-font-lock-keywords-2)))
;; (nvp:font-lock-add-defaults 'inferior-emacs-lisp-mode
;;   ((nvp:re-opt '("IELM error" "Eval error" "Read error")) .
;;    (0 font-lock-warning-face prepend))
;;   (:splice
;;    (mapcar #'nvp-comint-font-lock-keywords lisp-el-font-lock-keywords-2))
;;   (:splice
;;    (mapcar #'nvp-comint-font-lock-keywords lisp-cl-font-lock-keywords-2)))
;; (font-lock-add-keywords 'inferior-emacs-lisp-mode nvp-ielm-font-lock-keywords)

(provide 'nvp-ielm)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-ielm.el ends here
