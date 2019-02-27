;;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; URL: https://github.com/vividsnow/perl-live

;;; Code:
(require 'comint)
(require 'ansi-color)

;;* variables
(defvar perl-live-bin (executable-find "perl"))

(defvar perl-live-script
  (expand-file-name "../bin/perl-live.pl"))

(defvar perl-live-switches "-g") ; -g for globalize context

(defconst perl-live-name "perl live"
  "Perl live process name.")

(defconst perl-live-buffer (format "*%s*" perl-live-name)
  "Perl live buffer -- derived from `perl-live-name'.")

;;* functions
(defun perl-live-eval (text)
  "Eval `TEXT' in perl interpreter."
  (interactive "s")
  (process-send-string perl-live-name (format "%s\n" text))
  (process-send-eof perl-live-name))

(defun perl-live-eval-region (start end)
  "Evaluate region."
  (interactive "r")
  (perl-live-eval (buffer-substring-no-properties start end))
  (deactivate-mark))

(defun perl-live-eval-line ()
  "Evaluate line."
  (interactive)
  (save-excursion
    (let ((l (bounds-of-thing-at-point 'line)))
      (perl-live-eval-region (car l) (cdr l))))
  (forward-line))

(defun perl-live-eval-region-or-line ()
  "Evaluate line or region."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (perl-live-eval-region (region-beginning) (region-end))
    (perl-live-eval-line)))

(defun perl-live-eval-sexp ()
  "Eval current sexp."
  (interactive)
  (save-excursion
    (let ((s (bounds-of-thing-at-point 'sexp)))
      (perl-live-eval-region (1+  (car s)) (- (cdr s) 1)))))

(defun perl-live-run ()
  "Run perl-live comint session."
  (interactive)
  (if (string= (process-status perl-live-name) "run")
      (message "already run")
    (make-comint perl-live-name perl-live-bin nil perl-live-script
                 perl-live-switches)
    (with-current-buffer perl-live-buffer
      (mapc (lambda (v) (set (make-local-variable v) 't))
              (list 'ansi-color-for-comint-mode
                    'comint-scroll-to-bottom-on-input
                    'comint-scroll-to-bottom-on-output
                    'comint-move-point-for-output))
      (set (make-local-variable 'comint-input-sender)
           '(lambda (proc string)
              (comint-send-string proc (format "%s\n" string))
              (process-send-eof proc))))
    (start-process perl-live-name perl-live-buffer perl-live-bin
                   perl-live-script perl-live-switches)
    (message (format "check output at %s buffer" perl-live-buffer))))

(defun perl-live-stop ()
  "Stop perl-live session."
  (interactive)
  (delete-process perl-live-name)
  (message (format "process %s killed" perl-live-name)))

(defun perl-live-restart ()
  "Restart perl-live session."
  (interactive)
  (delete-process perl-live-name)
  (sleep-for 0 100)
  (perl-live-run))

(provide 'perl-live)
