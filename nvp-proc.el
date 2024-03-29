;;; nvp-proc.el --- process-related functions -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; - Async shell command w/ callback
;; - filters
;; - sentinels
;; - small subrs to find processes
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp:decls)

;;;###autoload
(defun nvp-async-shell-command-to-string (command &optional callback buffer)
  "Execute COMMAND as an `async-shell-command', running CALLBACK with results
if non-nil. Default just barfs output in message win or lose."
  (nvp:defq callback
    (lambda (p _m)
      (message
       (with-current-buffer (process-buffer p)
         (prog1 (string-trim-right (buffer-string))
           (kill-buffer (current-buffer)))))))
  (nvp:with-process command
    :proc-name "async-string"
    :proc-buff (or (and buffer (get-buffer-create buffer))
                   (generate-new-buffer-name " *temp*"))
    :shell t
    :callback callback))
(put 'nvp-async-shell-command-to-string 'lisp-indent-function 'defun)

;; -------------------------------------------------------------------
;;; Filters / Sentinels

;; condense multiple newlines
;;;###autoload
(defun nvp-proc-default-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert (replace-regexp-in-string "[\r\n]+" "\n" string))))

;; log proc & exit status. Kill buffer or RETURN-BUFFER it on success.
;; On failure, jump to process-buffer
;;;###autoload
(defun nvp-proc-default-sentinel (&optional return-buffer)
  (nvp:with-syms (p m buff pname)
    `(lambda (,p ,m)
       (let ((,pname (process-name ,p))
             (,buff (process-buffer ,p)))
         (funcall nvp-default-log-function "%s: %s" nil ,pname ,m)
         (unwind-protect
             (with-current-buffer ,buff
               (if (not (zerop (process-exit-status ,p)))
                   (progn (nvp-indicate-modeline ,pname 'failure)
                          (pop-to-buffer (current-buffer)))
                 (nvp-indicate-modeline ,pname)))
           ,(if return-buffer `,buff
              `(kill-buffer ,buff)))))))

(provide 'nvp-proc)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-proc.el ends here
