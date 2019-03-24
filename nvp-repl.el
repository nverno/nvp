;;; nvp-repl.el --- Generic REPL functions -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-23 23:24:27>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 22 March 2019

;;; Commentary:

;; Always switch to REPL in other window by default, same window
;; TODO:
;; - send-region
;; - send-dwim (region / previous sexp)
;; - send-defun
;; - redirect output

;; other options:
;; - https://github.com/tomterl/repl-toggle/blob/master/repl-toggle.el
;; - https://github.com/kaz-yos/eval-in-repl -- meh

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'subr-x)
  (require 'cl-lib))
(require 'nvp)
(require 'nvp-display)

;; -------------------------------------------------------------------
;;; Utils

;; find buffer with matching BUFNAME
(defsubst nvp-repl--match-name (bufname)
  (cl-find-if (lambda (buff) (string-match-p bufname (buffer-name buff))) (buffer-list)))

;; find buffer with major mode in MODES
(defun nvp-repl--match-mode (modes)
  (or (consp modes) (setq modes (cons modes nil)))
  (cl-loop for buff in (buffer-list)
     when (memq (buffer-local-value 'major-mode (get-buffer buff)) modes)
     return buff))

;; associate REPL buffer with SRC-BUFFER
(defsubst nvp-repl--associate (src-buffer)
  (let ((proc (nvp-buffer-process)))
    (when (not (process-get proc :src-buffer))
      (process-put proc :src-buffer src-buffer))))

;; -------------------------------------------------------------------
;;; Generics

(cl-defgeneric nvp-repl-get-buffer ()
  "Return a REPL buffer if one exists."
  (let* ((bufname (nvp-mode-val 'repl-buffer))
         (find-fn (nvp-mode-val 'repl-find-fn))
         (repl-modes (nvp-mode-val 'repl-mode 'all))
         (live-p (nvp-mode-val 'repl-live-p))
         (buff (if bufname (nvp-repl--match-name bufname)
                 (if find-fn (ignore-errors (funcall find-fn))
                   (nvp-repl--match-mode repl-modes)))))
    (if (and buff (if live-p (funcall live-p buff) (comint-check-proc buff)))
        buff                     ; return live REPL buffer
      ;; otherwise initialize a new one
      (or (nvp-repl-start)
          (user-error "Failed to initialize REPL")))))

(cl-defgeneric nvp-repl-start ()
  "Initialize and return a new REPL buffer associated with current buffer."
  (let ((wait (nvp-mode-val 'repl-wait))
        (init-fn (nvp-mode-val 'repl-init-fn))
        (src-buff (current-buffer))
        repl-buffer)
    (setq repl-buffer (funcall init-fn))
    (and wait (sit-for wait))
    (and (processp repl-buffer) (setq repl-buffer (process-buffer repl-buffer)))
    (nvp-repl--associate src-buff)
    repl-buffer
    ;; (with-current-buffer repl-buffer
    ;;   (when hist
    ;;     (if (derived-mode-p '(eshell-mode comint-mode term-mode))
    ;;         ;; setup REPL buffer to write history, hippie expand
    ;;         ;; FIXME: different config for each, but all can use
    ;;         ;; `nvp-comint-add-history-sentinel', move non-comint specific
    ;;         ;; functions to nvp-proc
    ;;         )))
    ))

;; -------------------------------------------------------------------
;;; Commands

;;;###autoload
(defun nvp-repl-jump (&optional action)
  "Jump between source and REPL buffers, staring if necessary."
  (interactive "P")
  (let ((buff (if-let ((proc (nvp-buffer-process)))
                  (or (process-get proc :src-buffer)
                      (other-buffer (current-buffer) 'visible))
                (nvp-repl-get-buffer))))
    (nvp-display-location buff :buffer action)))

(provide 'nvp-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-repl.el ends here
