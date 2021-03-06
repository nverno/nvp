;;; perl-reply.el --- interact with reply repl -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'comint)
(require 'perl-mode)
(require 'cperl-mode)

(defvar perl-reply-font-lock-keywords (cperl-load-font-lock-keywords-2))
(defvar perl-reply-program "reply")
(defvar perl-reply-filter-regexp "\\`\\s-+")
(defvar perl-reply-buffer "*reply*")

;; -------------------------------------------------------------------
;;; Process

;; return repl process / start one if needed
(defun perl-reply-process (&optional prompt)
  (let ((buff (get-buffer-create perl-reply-buffer)))
    (if (comint-check-proc buff)
        (get-buffer-process buff)
      (perl-reply-start prompt))))

(defun perl-reply-start (&optional prompt)
  "Create new REPL process and return it."
  (with-current-buffer 
      (apply #'make-comint "reply" perl-reply-program nil
             (if prompt
                 (split-string-and-unquote
                  (read-from-minibuffer "Reply switches: "))
               ()))
    (perl-reply-mode)
    (get-buffer-process (current-buffer))))

(defun perl-reply-input-filter (str)
  (not (string-match perl-reply-filter-regexp str)))

(defun perl-reply-send-string (str)
  (comint-send-string
   (perl-reply-process) 
   (replace-regexp-in-string "[\r\n]*$" " " str))
  (comint-send-string (perl-reply-process) "\n"))

;; -------------------------------------------------------------------
;;; Commands

(defun perl-reply-eval-region (start end)
  (interactive "r")
  (perl-reply-send-string (buffer-substring-no-properties start end)))

(defun perl-reply-eval-line ()
  (interactive)
  (perl-reply-eval-region (line-beginning-position) (line-end-position)))

(defun perl-reply-eval-buffer ()
  (interactive)
  (perl-reply-eval-region (point-min) (point-max)))

(defun perl-reply-eval-last-sexp ()
  (interactive))

(defun perl-reply-eval-defun ()
  (interactive))

;;;###autoload
(define-minor-mode perl-reply-minor-mode
  "Minor mode for perl reply REPL."
  :lighter " Reply"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-r") #'perl-reply-eval-region)
            (define-key map (kbd "C-x C-e") #'perl-reply-eval-last-sexp)
            (define-key map (kbd "C-c C-b") #'perl-reply-eval-buffer)
            (define-key map (kbd "M-C-x")   #'perl-reply-eval-defun)
            map))

;;;###autoload(defalias 'run-perl 'perl-reply-run)
;;;###autoload
(defun perl-reply-run (&optional prompt)
  (interactive "P")
  (prog1 (perl-reply-process prompt)
    (pop-to-buffer perl-reply-buffer)))

;;;###autoload
(define-derived-mode perl-reply-mode comint-mode "Reply"
  "Perl Reply REPL mode."
  (setq mode-line-process '(":%s"))
  (setq-local comint-prompt-regexp "^[0-9]+> *")
  (setq-local comint-input-filter 'perl-reply-input-filter)
  (setq-local font-lock-defaults '(perl-reply-font-lock-keywords t)))

(provide 'perl-reply)
;;; perl-reply.el ends here
