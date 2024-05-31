;;; nvp-repl-commands.el --- Run Repl commands -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Commands:
;; - `nvp-repl-cd'   :: Change repl working directory
;; - `nvp-repl-pwd'  :: Print repl working directory
;; - `nvp-repl-help' :: Repl help/help for thing with arg
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-repl)
(nvp:decls)


(eval-when-compile
  (defmacro nvp:with-current-repl (&rest body)
    `(if-let ((nvp-repl-current (nvp-repl-current)))
         (progn ,@body)
       (user-error "No current repl.")))

  (defmacro nvp:with-repl-vals (vals &rest body)
    (declare (indent 1))
    `(nvp:with-current-repl
      (pcase-let (((cl-struct nvp--repl ,@vals) nvp-repl-current))
        ,@body)))

  ;; Call `nvp-repl-send-string' with 'insert so input goes through
  ;; `comint-input-sender' in order to handle repl commands
  (defmacro nvp:call-repl-cmd (cmd &optional args &rest body)
    (declare (indent 2))
    `(nvp:with-repl-vals (,cmd)
       (if (null ,cmd) (user-error "not implemented")
         (progn (pcase ,cmd
                  ((pred stringp)
                   (nvp-repl-send-string
                    ,(if args `(format ,cmd ,@args) `,cmd) 'insert 'no-newline))
                  ((pred functionp) (funcall ,cmd ,@args))
                  ((pred symbolp) (eval ,cmd))
                  ,@(when args
                      `((`(:no-arg ,no-arg :with-arg ,with-arg)
                         (cl-assert (and (stringp no-arg) (stringp with-arg)))
                         (nvp-repl-send-string
                          (if ,@args (format with-arg ,@args) no-arg)
                          'insert 'no-newline))))
                  (_ (user-error
                      ,(concat "unhandled " (symbol-name cmd) " type: '%S'") ,cmd)))
                ,@body))))

  (defmacro nvp:with-repl-src-buffer (&rest body)
    (declare (indent defun) (debug t))
    (nvp:with-syms (buf)
      `(let ((,buf (nvp-repl-current-source-buffer)))
         (unless (and ,buf (buffer-live-p ,buf))
           (user-error "No source buffer associated with current buffer."))
         (with-current-buffer ,buf
           ,@body)))))

;;;###autoload
(defun nvp-repl-cd (&optional dir)
  "Set repl working directory to DIR (default `default-directory').
Prompt with \\[universal-argument]."
  (interactive
   (list (if current-prefix-arg
             (expand-file-name (read-directory-name "Directory: " default-directory))
           default-directory)))
  (unless dir (setq dir default-directory))
  (nvp:with-repl-src-buffer
    (let ((default-directory dir))
      (nvp:call-repl-cmd cd-cmd (default-directory)
        (nvp-repl-update (nvp--repl-repl-proc nvp-repl-current) (current-buffer))
        (with-current-buffer (nvp-repl-buffer)
          (setq default-directory dir))))))

;;;###autoload
(defun nvp-repl-pwd (&optional and-go)
  "Print repl current working directory/buffer."
  (interactive "P")
  (nvp:call-repl-cmd pwd-cmd nil
    (and and-go (pop-to-buffer (nvp-repl-buffer)))))

;;;###autoload
(defun nvp-repl-help (&optional thing and-go)
  "Print repl help for THING or repl."
  (interactive (if current-prefix-arg
                   (list (read-string "Help: " (thing-at-point 'symbol)) t)))
  (nvp:call-repl-cmd help-cmd (thing)
    (and and-go (pop-to-buffer (nvp-repl-buffer)))))

;;;###autoload
(defun nvp-repl-config (&optional alternative and-go)
  "Show repl config."
  (interactive (list current-prefix-arg))
  (nvp:call-repl-cmd config-cmd (alternative)
    (and and-go (pop-to-buffer (nvp-repl-buffer)))))

(provide 'nvp-repl-commands)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-repl-commands.el ends here
