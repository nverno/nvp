;;; perl-reply.el --- interact with reply repl -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-24 03:50:44>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/perl-tools
;; Created: 15 November 2016

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'cl-lib))

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
  (set-buffer
   (apply 'make-comint "reply" perl-reply-program nil
          (if prompt
              (split-string-and-unquote
               (read-from-minibuffer "Reply switches: "))
            ())))
  (perl-reply-mode)
  (get-buffer-process perl-reply-buffer))

(defun perl-reply-input-filter (str)
  (not (string-match perl-reply-filter-regexp str)))

(defun perl-reply-send-string (str)
  (comint-send-string
   (perl-reply-process)
   (replace-regexp-in-string "[\r\n]+$" " " (concat str "\n"))))

;; -------------------------------------------------------------------
;;; Commands

(defun perl-reply-send-region (start end)
  (interactive "r")
  (perl-reply-send-string
   (buffer-substring-no-properties start end)))

(defun perl-reply-send-line ()
  (interactive)
  (perl-reply-send-string
   (buffer-substring-no-properties (point-at-bol) (point-at-eol))))

(defun perl-reply-send-buffer ()
  (interactive)
  (perl-reply-send-string
   (buffer-substring-no-properties (point-min) (point-max))))

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
  (setq-local comint-input-filter 'perl-reply-input-filter))

(provide 'perl-reply)
;;; perl-reply.el ends here
