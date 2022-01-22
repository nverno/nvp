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
(nvp-decls)

;;;###autoload
(defun nvp-async-shell-command-to-string (command &optional callback)
  "Execute COMMAND as an `async-shell-command', running CALLBACK with results
if non-nil. Default just barfs output in message win or lose."
  (nvp-defq callback
    (lambda (p _m)
      (message
       (with-current-buffer (process-buffer p)
         (prog1 (string-trim-right (buffer-string))
           (kill-buffer (current-buffer)))))))
  (nvp-with-process command
    :proc-name "async-string"
    :proc-buff (generate-new-buffer-name " *temp*")
    :shell t
    :callback callback))
(put 'nvp-async-shell-command-to-string 'lisp-indent-function 'defun)

;; -------------------------------------------------------------------
;;; Find processes

;; find first item using TEST function (default 'equal)
(cl-defsubst nvp-proc-find (item &key test key)
  (declare (indent defun))
  (let ((completion-ignore-case t)
        (case-fold-search t))
    (cl-find item (process-list) :test (or test #'equal) :key key)))

;; find first process matched by PRED function
(cl-defsubst nvp-proc-find-if (pred &key key start end from-end)
  (declare (indent defun))
  (and pred (cl-find-if pred (process-list) :key key :start start :end end
                        :from-end from-end)))

;; find all processes matching PRED
(defsubst nvp-proc-find-all (pred)
  (cl-loop for proc in (process-list)
     when (funcall pred proc)
     collect proc))

;; find process by matching NAME
(defsubst nvp-proc-find-by-name (name)
  (nvp-proc-find name :test #'string-match-p :key #'process-name))

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
  (nvp-with-syms (p m buff pname)
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

;; generate and return a new comint buffer
(defsubst nvp-proc-comint-buffer (name)
  (with-current-buffer (generate-new-buffer name)
    (comint-mode)
    (current-buffer)))

(provide 'nvp-proc)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-proc.el ends here
