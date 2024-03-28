;;; nvp-ess.el --- ESS -*- lexical-binding: t; -*-
;;; Commentary:
;; stuff applicable across ESS dialects
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls :p (ess) :f (ess-get-process))
(require 'ess nil t)
(require 'ess-inf nil t)
(require 'ess-utils nil t)

;; Mark paragraphs, successively on repeated commands
;;;###autoload
(defun nvp-ess-mark-defun (&optional _arg)
  (interactive)
  (nvp:mark-defun
   ;; first mark
   (ess-mark-function-or-para)
   ;; repeated calls
   (condition-case nil
       (progn
         (forward-line 1)
         (end-of-defun))
     (error (forward-paragraph)))
   (point)))

(defun nvp-r-dev-off ()
  (interactive)
  (let ((proc (ess-get-process)))
    (ess-send-string proc "dev.off()")))

;;;###autoload
(defun nvp-r-redirect-output (command &optional buffer process)
  "Redirect REPL output to temp buffer."
  (interactive (list (read-from-minibuffer "Command: ")))
  (let ((buff (get-buffer-create
               (or buffer
                   (and current-prefix-arg
                        (concat "*"
                                (read-from-minibuffer "Output buffer: ")
                                "*"))
                   "*r-output*")))
        ;; `ess-get-process' defaults to process local to current
        ;; buffer, so to call from anywhere default to "R"
        (proc (ess-get-process (or process "R"))))
    ;; send a trailing newline to process
    (unless (string-match-p "\n$" command)
      (setq command (concat command "\n")))
    (ess-command command buff 'sleep nil nil proc)
    (with-current-buffer buff
      ;; process stuff
      (pop-to-buffer buff))))

;;;###autoload
(defun nvp-ess-process-abort (arg)
  (interactive "P")
  (let ((proc (ess-get-process)))
    (when proc
      (if arg
          (kill-process proc)
        (interrupt-process proc)))))

;; Evaluate active region or line.
;;;###autoload
(defun nvp-ess-eval ()
  (interactive)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'ess-eval-region)
    (ess-eval-line-and-step)))

;; -------------------------------------------------------------------
;;; Help at point

;;;###autoload
(defun nvp-hap-ess (command &optional arg &rest _args)
  (cl-case command
    (thingatpt (ess-symbol-at-point))
    (doc-buffer
     (condition-case-unless-debug err
         (save-window-excursion
           (ess-display-help-on-object (symbol-name arg))
           (list (current-buffer) nil))
       (error (message "%s" (error-message-string err))
              nil)))))

(provide 'nvp-ess)
;;; nvp-ess.el ends here
