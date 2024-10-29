;;; nvp-inf.el --- interactive process commands -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls)

;; completing read for processes
;;;###autoload
(defun nvp-inf-read-process ()
  (let* ((current (nvp:buffer-process))
         (prompt (if current (format "Process (default %s): " current)
                   "Process: ")))
   (when-let* ((procs (mapcar #'(lambda (p) (cons (process-name p) p)) (process-list))))
     (cdr (assoc-string
           (nvp-completing-read
            prompt (mapcar #'car procs) nil nil nil nil current)
           procs)))))

(eval-when-compile
  (defmacro nvp:inf-read-or-current (arg)
    "With ARG prompt for process, otherwise use current buffer's process."
    `(if ,arg (nvp-inf-read-process) (nvp:buffer-process)))

  (defmacro nvp:inf-message (proc var)
    "Display message about PROC's VAR."
    (macroexp-let2 nil proc proc
      `(message "%s %s" (or ,proc "Current buffer")
                (if ,proc (format ": %S" ,var) "has no process")))))

;; -------------------------------------------------------------------
;;; Commands

;;;###autoload
(defun nvp-jump-to-process-buffer (action proc)
  "Prompt for process name and display its buffer."
  (interactive (list current-prefix-arg (nvp-inf-read-process)))
  (if proc (nvp-with-display-actions action
             (pop-to-buffer (process-buffer proc)))
    (user-error "No processes")))

;;-- process info

;;;###autoload
(defun nvp-inf-process-status (arg)
  "Print message with process's status, prompting for process with ARG."
  (interactive "P")
  (let ((proc (nvp:inf-read-or-current arg)))
    (nvp:inf-message proc (process-status proc))))

;;;###autoload
(defun nvp-inf-process-attributes (proc var)
  (interactive
   (list (nvp:inf-read-or-current t)
         (nvp-completing-read "Process attr (default plist): "
                              '("filter" "plist" "coding" "exit" "command" "id"
                                "name" "sentinel")
                              nil nil nil nil "plist")))
  (let ((res (pcase var
               (`"filter" (process-filter proc))
               (`"plist" (process-plist proc))
               (`"coding" (process-coding-system proc))
               (`"exit" (process-exit-status proc))
               (`"command" (process-command proc))
               (`"id" (process-id proc))
               (`"name" (process-name proc))
               (`"sentinel" (process-sentinel proc))
               (_ (condition-case var
                      (process-get proc var)
                    (error (format "Process attribute %s not found" var)))))))
    (if (and res (string= var "plist"))
        (nvp:with-results-buffer :buffer (help-buffer)
          (pp res))
      (nvp:inf-message proc res))))

(provide 'nvp-inf)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-inf.el ends here
