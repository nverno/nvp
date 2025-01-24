;;; nvp-backtrace.el --- Backtrace -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'elisp-mode)
(require 'backtrace)
(nvp:decls)

;;; Completion
;; Completion table for frame locals in *Backtrace*
;; Gathers locals relevant to frame at point: this is locals in the frames at
;; higher indices (not including index at point)
(defun nvp-backtrace--local-variables (_string)
  (when backtrace-frames
    (--mapcat
     (-some->> (backtrace-frame-locals it) (--map (car it)))
     (nthcdr (1+ (backtrace-get-index)) backtrace-frames))))

(defvar nvp-backtrace-locals-completion-table
  (completion-table-dynamic
   #'nvp-backtrace--local-variables 'do-switch-buffer)
  "Completion table for frame locals in *Backtrace*.")

;; Override interactive spec of `debugger-eval-expression' to use
;; `nvp@backtrace-eval' instead
(defun nvp@backtrace-eval (orig-fn &rest args) ; exp &optional nframes
  (interactive
   (let ((elisp--local-variables-completion-table
          nvp-backtrace-locals-completion-table))
     (list (read--expression "[nvp] Eval in stack frame: "))))
  (apply orig-fn args))

(advice-add 'debugger-eval-expression :around #'nvp@backtrace-eval)

;; Starts off as single-line
(defvar-local nvp-backtrace--multi t)

(defun nvp-backtrace-toggle-multi ()
  (interactive)
  (if (nvp:toggle-variable nvp-backtrace--multi)
      (backtrace-single-line)
    (backtrace-multi-line)))

(provide 'nvp-backtrace)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-backtrace.el ends here
