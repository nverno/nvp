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
       (and current-prefix-arg (pop-to-buffer (current-buffer)))))))

;; ielm-return always wants to eval when smartparens close sexps
(defun nvp-ielm-nl (&optional arg)
  "Insert nl and indent unless point is at end-of-line."
  (interactive "P")
  (if (eolp)
      (ielm-return arg)
    (let (ielm-dynamic-return)
      (newline-and-indent))))

(defun nvp-ielm-help (&rest _)
  (interactive "P")
  (save-window-excursion (describe-mode ielm-working-buffer))
  (display-buffer (help-buffer)))

(defun nvp-ielm-cd (&optional _arg)
  (interactive "P")
  (if (eq major-mode 'inferior-emacs-lisp-mode)
      (--if-let (gethash (current-buffer) nvp-repl--process-buffers)
          (and (buffer-live-p it)
               (ielm-change-working-buffer it))
        (message "%s not asociated with a source buffer" (current-buffer)))
    (ielm-change-working-buffer (current-buffer))))

;; Send STR to ielm without inserting into repl
(defun nvp-ielm-send-string (_proc str &optional for-effect)
  (with-current-buffer (nvp-repl-current-buffer)
    (ielm-eval-input str for-effect)))

(with-eval-after-load 'nvp-repl
  (nvp-repl-add '(emacs-lisp-mode lisp-data-mode lisp-interaction-mode)
    :name 'ielm
    :modes '(inferior-emacs-lisp-mode)
    :init #'ielm
    :procname "ielm"
    :wait 0.1
    :history-file ".ielm_history"
    :eval-filter (lambda (s) (replace-regexp-in-string "[ \n\t]+" " " s))
    :send-input #'ielm-send-input
    :send-string #'nvp-ielm-send-string
    :help-cmd #'nvp-ielm-help
    :pwd-cmd #'ielm-print-working-buffer
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
