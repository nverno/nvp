;;; nvp-ielm.el --- ielm/lisp-interaction -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'lisp-mode)
(require 'ielm)
(nvp:decls :p (nvp-repl) :f (nvp-repl-buffer) :v (nvp-repl--process-buffers))


(define-advice ielm (:around (orig-fn &optional buf-name) "pop-to-buffer")
  (let* ((src-buf (current-buffer))
         (buf-name (or buf-name "*ielm*"))
         (ielm-buf
          (nvp:with-letf 'pop-to-buffer-same-window #'ignore
            (funcall orig-fn buf-name)
            (get-buffer buf-name))))
    (cl-assert (buffer-live-p ielm-buf))
    (with-current-buffer ielm-buf
      (setq ielm-working-buffer src-buf)
      (prog1 ielm-buf
        (or (get-buffer-window ielm-buf)
            (pop-to-buffer ielm-buf))))))

;; `ielm-return' always wants to eval when smartparens close sexps
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
     (cond ((null arg) #'describe-mode)
           ((fboundp arg) #'describe-function)
           ((boundp arg) #'describe-variable)
           (t (user-error "unbound symbol: '%S'" arg)))
     (or arg (nvp-repl-buffer))))
  (prog1 t (display-buffer (help-buffer))))

(defun nvp-ielm-cd (&optional directory buffer)
  "Change repl working BUFFER and/or DIRECTORY."
  (interactive (list nil (read-buffer "Set source buffer: ")))
  (or buffer (and directory (setq buffer (current-buffer))))
  (or directory buffer
      (user-error "no directory or buffer to change"))
  (when-let* ((repl-buf (nvp-repl-buffer)))
    (when directory
      (unless (file-exists-p directory)
        (user-error "Bad directory \"%S\"" directory))
      (with-current-buffer (get-buffer buffer)
        (setq default-directory directory)))
    (when buffer
      (with-current-buffer repl-buf
        (ielm-change-working-buffer buffer))
      (nvp-ielm-pwd nil))))

(defun nvp-ielm-pwd (&optional verbose)
  "Print working buffer and directory in the ielm repl.
When VERBOSE, print working buffer in echo area."
  (interactive (list t))
  (when-let* ((repl-buf (nvp-repl-buffer)))
    (with-current-buffer repl-buf
      (let ((str (format
                  "ielm-working-buffer: %s\\ndefault-directory: %s"
                  (buffer-name ielm-working-buffer)
                  (buffer-local-value
                   'default-directory ielm-working-buffer))))
        (ielm-eval-input (format "(princ \"\n%s\n\")" str) t))
      (when verbose
        (ignore (ielm-print-working-buffer))))))

(defun nvp-ielm-send-string (_proc str &optional for-effect)
  "Send STR to ielm without inserting into repl."
  (with-current-buffer (nvp-repl-buffer)
    (ielm-eval-input str for-effect)))


(with-eval-after-load 'nvp-repl
  (nvp-repl-add
    '( emacs-lisp-mode lisp-data-mode lisp-interaction-mode
       debugger-mode apropos-mode command-history-mode help-mode)
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

(provide 'nvp-ielm)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-ielm.el ends here
