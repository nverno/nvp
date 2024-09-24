;;; nvp-racket-repl.el --- Racket repl -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; - sending REPL regions from different modules:
;;   https://stackoverflow.com/questions/55546058/racket-mode-can-i-evaluate-a-single-form-within-a-given-namespace-at-the-repl
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-repl)
(nvp:decls :p (racket) :f (nvp-repl-show-result))

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

(defun nvp-racket-eval-last-sexp (&optional insert)
  "Evaluate last sexp and show results in overlay.
With prefix INSERT, insert results in buffer at point instead."
  (interactive "P")
  (nvp-racket-eval-string
   (buffer-substring-no-properties
    (racket--start-of-previous-expression) (point))
   insert))

(defun nvp-racket-eval-string (str &optional insert)
  "Evaluate STR in racket REPL and show the result."
  (interactive "sEval: \nP")
  (racket--assert-sexp-edit-mode)
  (unless (racket--repl-session-id)
    (user-error "No REPL session available; run the file first"))
  (racket--cmd/async (racket--repl-session-id)
                     `(eval ,str)
                     (lambda (v)
                       (setq this-command 'nvp-racket-eval-string)
                       (nvp-repl-show-result v insert))))


(when (fboundp 'racket-run-and-switch-to-repl)
  (nvp-repl-add '(racket-mode)
    :name 'racket
    :modes '(racket-repl-mode)
    :live #'numberp
    :process-p #'numberp
    :buf->proc (lambda (buf) (with-current-buffer buf (racket--repl-session-id)))
    :proc->buf #'nvp-racket--repl-buffer
    :wait 0.1
    :init #'nvp-racket--repl-init
    :clear-buffer #'racket-repl-clear-leaving-last-prompt
    :send-input nil
    :send-string #'ignore
    :eval-sexp #'nvp-racket-eval-last-sexp
    :eval-string #'nvp-racket-eval-string
    :send-region #'nvp-racket-send-region
    :send-defun #'racket-send-definition
    :send-sexp #'racket-send-last-sexp
    :send-buffer #'nvp-racket-send-buffer
    :help-cmd #'racket-repl-describe))

(provide 'nvp-racket-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-racket-repl.el ends here
