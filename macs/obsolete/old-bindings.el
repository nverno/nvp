
(cl-defmacro nvp-use-transient-bindings
    (&optional bindings
               &key
               (keep t)
               (pre '(nvp-indicate-cursor-pre))
               (exit '(lambda () (nvp-indicate-cursor-post)))
               (repeat t) ;add repeat binding as last char
               repeat-key) ;key to use instead of last char
  "Set BINDINGS in transient map or REPEAT command.
If both BINDINGS and REPEAT are nil, do nothing.
Run PRE form prior to setting commands and EXIT on leaving transient map.
If REPEAT is non-nil, add a binding to repeat command from the last input char
or use REPEAT-KEY if specified."
  (declare (indent defun) (debug defun))
  (when (or bindings repeat)            ;w/o one of these, do nothing
    (let ((msg (nvp--msg-from-bindings bindings)))
      `(progn
         (nvp:declare "" nvp-indicate-cursor-pre nvp-indicate-cursor-post)
         ;; only set first time
         (when (null overriding-terminal-local-map)
           (let ((tmap (make-sparse-keymap))
                 (repeat-key ,(when repeat (or repeat-key `(nvp:input 'lcs)))))
             ,(when repeat
                (prog1 nil
                  (setq msg (concat msg (and bindings ", ") "[%s] repeat command"))))
             (message ,msg repeat-key)   ;echo bindings on first pass
             ,pre
             ,@(cl-loop for (k . b) in bindings
                  collect `(nvp-bind tmap ,k ,b))
             ,(when repeat '(define-key tmap (kbd repeat-key) this-command))
             (set-transient-map
              tmap
              ,(if repeat `(lambda ()         ;conditions to continue
                             (when (and (not (memq this-command
                                                   '(handle-switch-frame
                                                     keyboard-quit)))
                                        (lookup-key tmap (this-single-command-keys)))
                               (message ,msg repeat-key) t))
                 `,keep)
              ,exit)))))))


(cl-defmacro nvp-with-temp-bindings ((&key (keep t) exit bindings)
                                     &rest body)
  "Execute BODY with BINDINGS set in transient map."
  (declare (indent 0))
  (let ((tmap (cl-gensym)))
    `(let ((,tmap (make-sparse-keymap)))
       ,@(cl-loop for (k . b) in bindings
            collect `(nvp-bind ,tmap ,k ,b))
       (set-transient-map ,tmap ,keep ,exit)
       ,@body)))

(defmacro nvp-with-view-bindings (&rest body)
  `(nvp-with-temp-bindings
     (:bindings
      ,(eval-when-compile nvp--bindings-view)
      :keep t
      :exit (lambda () (message "vim out")))
     ,@body))
