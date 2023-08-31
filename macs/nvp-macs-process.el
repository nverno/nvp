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

(defmacro nvp:buffer-process (&optional buffer)
  "Return BUFFER's process."
  `(get-buffer-process ,(or buffer '(current-buffer))))

(defmacro nvp:with-buffer-proc (proc &rest body)
  "Execute BODY with current buffer PROC if process is live."
  (declare (indent defun) (debug t))
  `(if-let ((,proc (nvp:buffer-process)))
       (if (not (process-live-p ,proc))
           (user-error "Buffer process is not live.")
         ,@body)
     (user-error "Current buffer has no process.")))

(defalias 'nvp:with-comint-buffer 'nvp:comint-buffer)
(cl-defmacro nvp:comint-buffer (&rest body &key name new result &allow-other-keys)
  "Get a new comint buffer from NAME and execute BODY there, returning buffer.
If NEW is non-nil, use `generate-new-buffer', otherwise `get-buffer-create'.
If RESULT is non-nil, return result of BODY instead of buffer."
  (declare (indent defun) (debug t))
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

(defmacro nvp:with-process-filter (process &optional proc-filter)
  "Run processs with `nvp-proc-default-filter'.
Return process object."
  (declare (indent defun))
  (if (and proc-filter (eql proc-filter :none)) `,process
    (and (not proc-filter) (setq proc-filter ''nvp-proc-default-filter))
    (macroexp-let2* nil ((process process) (proc-filter proc-filter))
      `(prog1 ,process
         (set-process-filter ,process ,proc-filter)))))

(cl-defmacro nvp:with-process-log (process &key
                                           on-error
                                           on-success
                                           proc-filter
                                           (display-action t))
  "Log output in log buffer, if on-error is :pop-on-error, pop to log
if process exit status isn't 0."
  (declare (indent defun))
  (macroexp-let2* nil ((proc `(nvp:with-process-filter ,process ,proc-filter))
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

(cl-defmacro nvp:with-process
    (process
     &key
     proc-name
     ;; return process buffer - when set, takes priority over combination of
     ;; proc-bufname + buffer-fn
     proc-buff
     (proc-bufname `(concat "*" (or ,proc-name ,process) "*"))
     proc-args
     (buffer-fn 'get-buffer-create)
     sync                 ; run synchronously
     shell                ; call as shell command (inheriting env)
     callback             ; do this instead of sentinels
     on-success
     on-failure
     (proc-filter :default)
     (proc-sentinel :default))
  "Start PROCESS with a sentinel doing ON-SUCCESS or ON-FAILURE in process
buffer. For async calls (default), returns process object.

Note for `call-process-shell-command', invoked with SHELL and SYNC being
non-nil, there is no process object to return. Instead, if ON-SUCCESS is
non-nil, it should be a function that will be called in the result buffer.
In this case, the return value is the exit status of the shell command.

\(fn PROCESS ...)"
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
                       ',proc-cmd (mapconcat
                                   'identity (list ,process ,@proc-args) " ")
                       nil ,(and on-success `,pbuf))
                   `(funcall ',proc-cmd ,(or proc-name process) ,pbuf
                             (mapconcat 'identity (list ,process ,@proc-args) " ")))
               `(funcall ',proc-cmd
                         ,(or proc-name process)
                         ,pbuf ,process ,@proc-args))))
       ,(if (and sync shell)
            (if on-success
                `(if (zerop ,proc)
                     (with-current-buffer ,pbuf
                       (funcall ,on-success))
                   ,(if on-failure `(funcall ,on-failure ,proc)
                      `,proc))
              (if on-failure `(funcall ,on-failure ,proc)
                (unless on-success `,proc)))
          `(progn
             ,(cond                         ; filter for sync/async
               ((memq proc-filter '(:default t))
                (unless sync
                  `(set-process-filter ,proc 'nvp-proc-default-filter)))
               (proc-filter
                `(set-process-filter ,proc ,proc-filter))
               (t nil))
             ,(unless sync               ; no sentinel for sync
                (cond
                 (callback `(set-process-sentinel ,proc ,callback))
                 ((or on-success on-failure)
                  `(set-process-sentinel
                    ,proc
                    (lambda (p m)
                      (nvp-log "%s: %s" nil (process-name p) m)
                      (with-current-buffer (process-buffer p)
                        (if (zerop (process-exit-status p))
                            ,on-success
                          ,on-failure)))))
                 ((memq proc-sentinel '(:default t))
                  `(set-process-sentinel ,proc (nvp-proc-default-sentinel)))
                 (proc-sentinel `(set-process-sentinel ,proc ,proc-sentinel))
                 ((null proc-sentinel) `,proc)))
             ;; return process object for async calls (exit code for sync)
             ,proc)))))

;; -------------------------------------------------------------------
;;; Wrappers / Overrides

(defmacro nvp:with-process-wrapper (wrapper &rest body)
  "Wrap `set-process-sentinel' to so BODY is executed in environment
where WRAPPER has effect, eg. `cl-letf' will have effect.
Note: use lexical-binding."
  (nvp:with-syms (setter proc sentinel)
    (macroexp-let2 nil wrapper wrapper
      `(let ((,setter (symbol-function 'set-process-sentinel)))
         (cl-letf (((symbol-function 'set-process-sentinel)
                    (lambda (,proc ,sentinel)
                      (funcall ,setter ,proc
                               ;; original sentinel is wrapped with any new
                               ;; bindings introduced in WRAPPER
                               (funcall ,wrapper ,sentinel)))))
           ,@body)))))

(defmacro nvp:with-async-override (orig-fn new-fn &rest body)
  "Set `symbol-function' of ORIG-FN to NEW-FN in process-buffer of BODY."
  (declare (indent defun))
  `(nvp:with-process-wrapper
    (lambda (fn)
      (let ((fun fn))
        (lambda (p m)
          (cl-letf (((symbol-function ,orig-fn) ,new-fn))
            ;; call original sentinel, FUN
            (funcall fun p m)))))
    ,@body))

(provide 'nvp-macs-process)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-macs-process.el ends here
