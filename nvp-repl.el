;;; nvp-repl.el --- Generic REPL functions -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-22 21:32:26>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 22 March 2019

;;; Commentary:

;; Always switch to REPL in other window
;; TODO:
;; - REPL buffer init - returns REPL buffer:
;;   - start REPL process if not running
;;   - add sentinels
;;   - configure history file
;;   - setup hippie expand from history
;;   - associate source buffer from which REPL was initiated
;; - switch b/w REPL and associated source buffer:
;;   - from REPL, switch to associated source or other-buffer by default
;;   - from source, switch to/init REPL in other-window
;; - send-region
;; - send-dwim (region / previous sexp)
;; - send-defun
;; - redirect output

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'nvp)

(defmacro nvp-mode-bind (&optional mode &rest bindings)
  "Attach BINDINGS globally to MODE."
  (declare (indent defun))
  (or mode (setq mode (quote major-mode)))
  `(if (not (get ,mode 'nvp))
       (put ,mode 'nvp ,@bindings)
     (put ,mode 'nvp
          (cl-delete-duplicates (append (get ,mode 'nvp) ,@bindings) :key #'car))))

;; return MODE value associated with KEY if exists
(define-inline nvp-mode-get-val (key &optional mode)
  (inline-letevals ((mode (or mode (quote major-mode))) key)
    (inline-quote (assq ,key (get ,mode 'nvp)))))

;; return mode value, default to cadr (first value minus the key)
(defsubst nvp-mode-val (key &optional all)
  (when-let* ((val (nvp-mode-get-val key)))
    (if all (cdr val)
      (cadr val))))

(nvp-mode-bind nil
  '((repl-mode   'inferior-emacs-lisp-mode)
    (repl-buffer "*ielm*")
    (repl-wait   0.1)
    (repl-history ".ielm_history")))

;; find buffer with matching BUFNAME
(defsubst nvp-repl--match-name (bufname)
  (cl-find-if (lambda (buff) (string-match-p bufname (buffer-name buff))) (buffer-list)))

;; find buffer with major mode in MODES
(defun nvp-repl--match-mode (modes)
  (or (consp modes) (setq modes (cons modes nil)))
  (cl-loop for buff in (buffer-list)
     when (memq (buffer-local-value 'major-mode (get-buffer buff)) modes)
     return buff))

(cl-defgeneric nvp-repl-get-buffer (&rest _args)
  "Return a REPL buffer if one exists."
  (let* ((bufname (nvp-mode-val 'repl-buffer))
         (find-fn (nvp-mode-val 'repl-find-fn))
         (repl-modes (nvp-mode-val 'repl-mode 'all))
         (live-p (nvp-mode-val 'repl-live-p))
         (buff (if bufname (nvp-repl--match-name bufname)
                 (if find-fn (ignore-errors (funcall find-fn))
                   (nvp-repl--match-mode repl-modes)))))
    (if (and buff (if live-p (funcall live-p buff) (comint-check-proc buff)))
        buff                            ;return live REPL buffer
      ;; otherwise initialize a new one
      (or (nvp-repl-init)
          (user-error "Failed to initialize REPL")))))

(cl-defgeneric nvp-repl-init (&rest _args)
  "Initialize and return a new REPL buffer."
  (let* ((bufname (nvp-mode-val 'repl-buffer))
         (wait (nvp-mode-val 'repl-wait))
         (live-p (nvp-mode-val 'repl-live-p))
         (hist (nvp-mode-val 'repl-history))
         (init-fn (nvp-mode-val 'repl-init-fn))
         repl-buffer)
    (setq repl-buffer (funcall init-fn))
    (and wait (sit-for wait))
    (and (processp repl-buffer) (setq repl-buffer (process-buffer repl-buffer)))
    (with-current-buffer repl-buffer
      (when hist
        (if (derived-mode-p '(eshell-mode comint-mode term-mode))
            ;; setup REPL buffer to write history, hippie expand
            ;; FIXME: different config for each, but all can use
            ;; `nvp-comint-add-history-sentinel', move non-comint specific
            ;; functions to nvp-proc
            )))))

(provide 'nvp-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-repl.el ends here
