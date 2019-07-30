;;; utop-inf ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'utop)

(defvar utop-inf-completions ())

(defsubst utop-inf-process ()
  (get-buffer-process utop-buffer-name))

;; call utop complete on STR, store completions in
;; `utop-inf-completions'
(defun utop-inf-redirect (str)
  (with-current-buffer utop-buffer-name
    (set-process-filter utop-process 'utop-inf-process-output)
    (utop-complete-input str)))

;; process utop output using `utop-inf-process-line'
;; when finished reset utops output filter
(defun utop-inf-process-output (_process output)
  (with-current-buffer utop-buffer-name
    (utop-perform
     (setq utop-output (concat utop-output output))
     (let ((lines (split-string utop-output "\n")))
       (while (>= (length lines) 2)
         (utop-inf-process-line (car lines))
         (setq lines (cdr lines)))
       (setq utop-output (car lines)))
     (set-process-filter utop-process 'utop-process-output))))

;; process utop output and push completions to `utop-inf-completions'
(defun utop-inf-process-line (line)
  (string-match "\\`\\([a-z-]*\\):\\(.*\\)\\'" line)
  (let ((command (match-string 1 line))
        (argument (match-string 2 line)))
    (cond
     ;; Complete with a word
     ((string= command "completion-word")
      (utop-set-state 'edit)
      (setq utop-inf-completions argument))
     ;; Start of completion
     ((string= command "completion-start")
      (setq utop-inf-completions nil))
     ;; A new possible completion
     ((string= command "completion")
      (push argument utop-inf-completions))
     ;; End of completion
     ((string= command "completion-stop")
      (utop-set-state 'edit)))))

(provide 'utop-inf)
;;; utop-inf.el ends here
