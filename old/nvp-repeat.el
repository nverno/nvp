;; -*- lexical-binding: t; -*-

(defvar nvp-repeat-key-enabled t)

(defun nvp-repeat-msg (repeat-key &optional bindings)
  (concat
   (format "[%S] repeat" repeat-key)
   (and bindings ", ")
   (mapconcat (lambda (b)
                (if-let* ((msg (plist-get b :msg)))
                    (format "[%S] %s" (car b) msg)
                  (pcase (cadr b)
                    (`(,(or 'function 'quote) ,sym)
                     (if (symbolp sym)
                         (format "[%S] %S" (car b) sym)
                       (format "[%S]" (car b))))
                    (_ (format "[%S]" (car b))))))
              bindings ", ")))

;; enable transient map for calling command
;; defaults to last basic char (no caps)
(defun nvp-repeat-command (&optional key no-indicator bindings &rest args)
  (when (and nvp-repeat-key-enabled
             (null overriding-terminal-local-map)
             (not (memq this-command `(nvp-repeat-command ,last-command))))
    (let* ((repeat-key (or key (nvp:input 'lbi)))
           (repeat-key-str (single-key-description repeat-key)))
      (when repeat-key
        (unless no-indicator (nvp-indicate-cursor-pre))
        (set-transient-map
         (let ((map (make-sparse-keymap)))
           (define-key map (vector repeat-key)
                       (if args
                           `(lambda () (interactive)
                              (setq this-command ',this-command)
                              (funcall-interactively #',this-command ,@args))
                         this-command))
           (pcase-dolist (`(,k ,b) bindings)
             (define-key map (kbd k) (eval b)))
           map)
         t
         (unless no-indicator
           (lambda () (nvp-indicate-cursor-post)
             (setq prefix-arg nil))))
        (or (minibufferp) (message (nvp-repeat-msg repeat-key-str bindings)))))))

(put 'nvp-repeat-command 'lisp-indent-function 'defun)

