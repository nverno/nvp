(require 'cl-lib)

(defvar my-init-file "~/.emacs.d/etc/init-bare.el")

(defun find-load-point (lib &optional continue)
  "During the first `require' or `load', print `load-file-name' when defined.
Otherwise, dump a backtrace around the loading call.
If CONTINUE is non-nil, don't stop after first load."
  (let* ((lib-sym (intern lib))
         (lib-path (or (locate-library lib) lib))
         (load-syms (mapcar
                     (lambda (s)
                       (cons s (intern (format "%s@watch-%s" s lib-sym))))
                     '(require load)))
         (cleanup (unless continue
                    (cl-loop for (ls . n) in load-syms
                       collect `(advice-remove ',ls ',n)))))
    (pcase-dolist (`(,load-sym . ,name) load-syms)
      (advice-add
       load-sym :around
       (defalias `,name
         `(lambda (f sym &rest args)
            (when (or (equal sym ',lib-sym)
                      (and (stringp sym)
                           (or (string= sym ,lib)
                               (file-equal-p sym ',lib-path))))
              ,@cleanup
              (prin1 (or (and load-in-progress
                              (format "%s => %s" ',lib-sym load-file-name))
                         (backtrace))))
            (apply f sym args)))))))

(defun find-initial-load ()
  "Call with 'emacs -q -l /this/file.el -f find-initial-load'."
  (find-load-point "tramp" 'continue)
  (load my-init-file))

;; test that deferred requires still get reported
;; (defun my-f () (require 'subr-x))
;; (add-hook 'emacs-startup-hook #'my-f)
