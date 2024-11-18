;;; nvp-inf.el --- interactive process commands -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls)

;;;###autoload
(defsubst nvp-inf-read-process (&optional prompt use-current)
  "Completing read for processes.
When USE-CURRENT is non-nil, dont prompt if current buffer has a process."
  (or (and use-current
           (get-buffer-process (current-buffer)))
      (read-process-name (or prompt "Process"))))

;;;###autoload
(defun nvp-jump-to-process-buffer (action proc)
  "Prompt for process name and display its buffer."
  (interactive (list current-prefix-arg (nvp-inf-read-process)))
  (if proc (nvp-with-display-actions action
             (pop-to-buffer (process-buffer proc)))
    (user-error "No processes")))

;;;###autoload
(defun nvp-inf-process-status (&optional arg)
  "Print message with process's status.
Same prefixes as `nvp-inf-process-attributes'."
  (interactive "P")
  (funcall-interactively #'nvp-inf-process-attributes arg "status"))

;;;###autoload
(defun nvp-inf-process-attributes (&optional arg attr proc prompt)
  "With prefix \\[universal-argument], always prompt.
With prefix <= 0 or \\='-, pretty-print in results buffer."
  (interactive (list nil current-prefix-arg))
  (let* ((pretty (or (memq arg '(- 0))
                     (< (prefix-numeric-value arg) 0)))
         (proc (or proc (nvp-inf-read-process prompt (not (equal arg '(4))))))
         (attr (or attr (completing-read "Process attr (default plist): "
                          '("coding-system" "command" "exit-status" "filter"
                            "id" "name" "plist" "sentinel" "status")
                          nil nil nil nil "plist")))
         (fn (intern (format "process-%s" attr)))
         (val (funcall fn proc)))
    (if (or pretty
            (and val (eq fn 'process-plist)))
        (nvp:with-help-setup (nvp-inf-process-attributes 0 attr proc)
          (nvp:with-help-window
            :title (format "Process plist (%s)" (process-name proc))
            :buffer (help-buffer)
            (nvp-pp-form val :pp-buffer)))
      (message "%s : %S" proc val))))

(provide 'nvp-inf)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-inf.el ends here
