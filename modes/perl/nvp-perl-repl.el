;;; nvp-perl-repl.el --- interact with reply repl -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-24 03:07:00>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/perl-tools
;; Created: 15 November 2016

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'cl-lib))

(defvar nvp-perl-repl-program "reply")
(defvar nvp-perl-repl-filter-regexp "\\`\\s-+")
(defvar nvp-perl-repl-buffer "*reply*")

;; -------------------------------------------------------------------
;;; Process

;; return repl process / start one if needed
(defun nvp-perl-repl-process ()
  (let ((buff (get-buffer-create nvp-perl-repl-buffer)))
    (if (comint-check-proc buff)
        (get-buffer-process buff)
      (nvp-perl-repl-run))))

(defun nvp-perl-repl-input-filter (str)
  (not (string-match nvp-perl-repl-filter-regexp str)))

(defun nvp-perl-repl-send-string (str)
  (comint-send-string
   (nvp-perl-repl-process)
   (replace-regexp-in-string "[\r\n]+$" " " (concat str "\n"))))

;; -------------------------------------------------------------------
;;; Commands

(defun nvp-perl-repl-send-region (start end)
  (interactive "r")
  (nvp-perl-repl-send-string
   (buffer-substring-no-properties start end)))

(defun nvp-perl-repl-send-line ()
  (interactive)
  (nvp-perl-repl-send-string
   (buffer-substring-no-properties (point-at-bol) (point-at-eol))))

(defun nvp-perl-repl-send-buffer ()
  (interactive)
  (nvp-perl-repl-send-string
   (buffer-substring-no-properties (point-min) (point-max))))

;; switch between repl and sending buffer
(defvar nvp-perl-repl--last-buffer)
;;;###autoload
(defun nvp-perl-repl-switch-buffers ()
  (interactive)
  (if (and (eq major-mode 'perl-repl-mode)
           nvp-perl-repl--last-buffer)
      (pop-to-buffer nvp-perl-repl--last-buffer)
    (setq nvp-perl-repl--last-buffer (current-buffer))
    (pop-to-buffer
     (process-buffer (nvp-perl-repl-process)))))

;;;###autoload(defalias 'run-perl 'nvp-perl-repl-run)
;;;###autoload
(defun nvp-perl-repl-run (&optional arg)
  (interactive "P")
  (unless (comint-check-proc nvp-perl-repl-buffer)
    (set-buffer
     (apply 'make-comint "reply" nvp-perl-repl-program nil
            (if arg
                (split-string-and-unquote
                 (read-from-minibuffer "Reply switches: "))
              ())))
    (nvp-perl-repl-mode)
    (pop-to-buffer nvp-perl-repl-buffer))
  (get-buffer-process nvp-perl-repl-buffer))

(defvar nvp-perl-repl-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "C-c C-z") #'nvp-perl-repl-switch-buffers)
    km))

;;;###autoload
(define-derived-mode nvp-perl-repl-mode comint-mode "Reply"
  "Perl Reply REPL mode."
  (setq mode-line-process '(":%s"))
  (setq-local comint-prompt-regexp "^[0-9]+> *")
  (setq-local comint-input-filter 'nvp-perl-repl-input-filter))

(provide 'nvp-perl-repl)
;;; nvp-perl-repl.el ends here
