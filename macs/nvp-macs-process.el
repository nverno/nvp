;;; nvp-macs-process.el --- process wrappers -*- lexical-binding: t; -*-

;;; Commentary:

;; Functions called:
;; `nvp-proc-default-filter'
;; `nvp-proc-default-sentinel'
;; `nvp-log'
;; `nvp-indicate-modeline'

;;; Code:
(require 'nvp-macs-common)

;; -------------------------------------------------------------------
;;; Buffers / Processes 

(defmacro nvp-buffer-process (&optional buffer)
  "Return BUFFER's process."
  `(get-buffer-process ,(or buffer '(current-buffer))))

(defalias 'nvp-with-comint-buffer 'nvp-comint-buffer)
(cl-defmacro nvp-comint-buffer (&rest body &key name new result &allow-other-keys)
  "Get a new comint buffer from NAME and execute BODY there, returning buffer.
If NEW is non-nil, use `generate-new-buffer', otherwise `get-buffer-create'.
If RESULT is non-nil, return result of BODY instead of buffer."
  (declare (indent defun) (debug body))
  (while (keywordp (car body))
    (setq body (cdr (cdr body))))
  (or name (setq name "*nvp*"))
  (let ((gen-fn (if new #'generate-new-buffer #'get-buffer-create)))
    `(progn (with-current-buffer (,gen-fn ,name)
              (comint-mode)
              ,(if (not result)
                   `(prog1 (current-buffer)
                      ,@body))))))

;; -------------------------------------------------------------------
;;; Helpers

(defmacro nvp-with-process-filter (process &optional proc-filter)
  "Run processs with `nvp-proc-default-filter'.
Return process object."
  (declare (indent defun))
  (if (and proc-filter (eql proc-filter :none)) `,process
    (and (not proc-filter) (setq proc-filter ''nvp-proc-default-filter))
    (macroexp-let2* nil ((process process) (proc-filter proc-filter))
      `(prog1 ,process
         (set-process-filter ,process ,proc-filter)))))

(cl-defmacro nvp-with-process-log (process &key
                                           on-error
                                           on-success
                                           proc-filter
                                           (display-action t))
  "Log output in log buffer, if on-error is :pop-on-error, pop to log
if process exit status isn't 0."
  (declare (indent defun))
  (macroexp-let2* nil ((proc `(nvp-with-process-filter ,process ,proc-filter))
                       (on-err (if (keywordp on-error)
                                   ;; (equal on-error :pop-on-error)
                                   `(pop-to-buffer (process-buffer ,proc)
                                                   ,display-action)
                                 on-error)))
    `(progn
       (set-process-sentinel ,proc
                             #'(lambda (p m)
                                 (nvp-log "%s: %s" nil (process-name p) m)
                                 (if (zerop (process-exit-status p))
                                     ,on-success
                                   ,on-err)))
       (display-buffer (process-buffer ,proc) ,display-action))))

;; -------------------------------------------------------------------
;;; Main wrapper

(cl-defmacro nvp-with-process
    (process
     &key
     (proc-name process)
     ;; return process buffer - when set, takes priority over combination of
     ;; proc-bufname + buffer-fn
     proc-buff
     (proc-bufname `,(concat "*" proc-name "*"))
     (proc-args nil)
     (buffer-fn 'get-buffer-create)
     sync                               ; run synchronously
     shell                              ; call as shell command (inheriting env)
     (proc-filter :default)
     (proc-sentinel :default)
     (on-success `(progn
                    (nvp-indicate-modeline-success
                     ,(concat " " proc-name " success"))
                    (kill-buffer (current-buffer))))
     (on-failure '(pop-to-buffer (current-buffer))))
  "Start PROCESS with a sentinel doing ON-SUCCESS or ON-FAILURE in process buffer."
  (declare (indent defun) (debug t))
  (let* ((proc (make-symbol "proc"))
         (proc-cmd (intern (format "%s%s"
                                   (if sync "call-process" "start-process")
                                   (if shell "-shell-command" ""))))
         (pbuf (cond
                (proc-buff `,proc-buff)
                ((and buffer-fn proc-bufname) `(,buffer-fn ,proc-bufname))
                (t `,proc-bufname))))
    `(let ((,proc
            ,(if shell
                 (if sync
                     `(funcall
                       ',proc-cmd (mapconcat 'identity (list ,@proc-args) " "))
                   `(funcall ',proc-cmd ,process ,pbuf
                             (mapconcat 'identity (list ,@proc-args) " ")))
               `(funcall ',proc-cmd
                         ,(or proc-name process)
                         ,pbuf ,process ,@proc-args))))
       (progn
         ,(cond            ; filter for sync/async
           ((memq proc-filter '(:default t))
            `(set-process-filter ,proc 'nvp-proc-default-filter))
           (proc-filter
            `(set-process-filter ,proc ,proc-filter))
           (t nil))
         ,(if sync `,proc  ; no sentinel for sync
            (cond
             ((memq proc-sentinel '(:default t))
              `(set-process-sentinel ,proc (nvp-proc-default-sentinel)))
             (proc-sentinel `(set-process-sentinel ,proc ,proc-sentinel))
             ((or on-success on-failure)
              `(set-process-sentinel ,proc
                                     (lambda (p m)
                                       (nvp-log "%s: %s" nil (process-name p) m)
                                       (with-current-buffer (process-buffer p)
                                         (if (zerop (process-exit-status p))
                                             ,on-success
                                           ,on-failure)))))
             ((null proc-sentinel) `,proc)))))))

;; -------------------------------------------------------------------
;;; Wrappers / Overrides

(defmacro nvp-with-process-wrapper (wrapper &rest body)
  "Wrap `set-process-sentinel' to so BODY is executed in environment
where WRAPPER has effect, eg. `cl-letf' will have effect.
Note: use lexical-binding."
  (let ((sps (cl-gensym))
        (proc (cl-gensym))
        (fn (cl-gensym)))
    (macroexp-let2 nil wrapper wrapper
      `(let ((,sps (symbol-function 'set-process-sentinel)))
         (cl-letf (((symbol-function 'set-process-sentinel))
                   (lambda (,proc ,fn)
                     (funcall ,sps ,proc (funcall wrapper ,fn))))
           ,@body)))))

(defmacro nvp-with-async-override (orig-fn new-fn &rest body)
  "Set `symbol-function' of ORIG-FN to NEW-FN in process-buffer of
BODY."
  (declare (indent defun))
  (macroexp-let2 nil new-fn new-fn
   `(nvp-with-process-wrapper
     (lambda (fn)
       (let ((fun fn))
         (lambda (p m)
           (cl-letf (((symbol-function ,orig-fn) new-fn))
             (funcall fun p m)))))
     ,@body)))

(provide 'nvp-macs-process)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-macs-process.el ends here
