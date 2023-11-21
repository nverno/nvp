;;; nvp-racket-repl.el --- Racket repl -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-repl)
(nvp:decls :p (racket))

(defun nvp-racket-send-buffer (&optional _and-go)
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp))
                (looking-at-p "[ \t]*$\\|^#"))
      (forward-line))
    (racket--send-region-to-repl (point) (point-max))))

;; `racket-send-region' but dont error if region isnt active
(defun nvp-racket-send-region (start end)
  (interactive "r")
  (racket--assert-edit-mode)
  (racket--send-region-to-repl start end))

;; return repl buffer for session ID
(defun nvp-racket--repl-buffer (id)
  (seq-some (lambda (buf)
              (when (buffer-live-p buf)
                (with-current-buffer buf
                  (when (and (eq major-mode 'racket-repl-mode)
                             (eq racket--repl-session-id id))
                    (current-buffer)))))
            (buffer-list)))

(defun nvp-racket--repl-init (&optional prefix)
  "Start racket repl.
With \\[universal-argument] \\[universal-argument] PREFIX instrument for
debugging."
  (interactive "P")
  (save-window-excursion
    (racket-run-and-switch-to-repl prefix)
    (get-buffer racket-repl-buffer-name)))

(when (fboundp 'racket-run-and-switch-to-repl)
  (nvp-repl-add '(racket-mode)
    :name 'racket
    :modes '(racket-repl-mode)
    :live #'numberp
    :process-p #'numberp
    :buff->proc (lambda (buf) (with-current-buffer buf (racket--repl-session-id)))
    :proc->buff #'nvp-racket--repl-buffer
    :clear-buffer nil
    :eval-sexp #'racket-eval-last-sexp
    :send-input nil
    :send-string #'ignore
    :send-region #'nvp-racket-send-region
    :send-defun #'racket-send-definition
    :send-sexp #'racket-send-last-sexp
    :send-buffer #'nvp-racket-send-buffer
    :wait 0.1
    :init #'nvp-racket--repl-init
    :help-cmd #'racket-repl-describe))

(provide 'nvp-racket-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-racket-repl.el ends here
