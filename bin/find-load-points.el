;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar my-init-file "~/.emacs.d/init.elc")

(defun find-load-point (lib &optional continue)
  "During the first `require' or `load', print `load-file-name' when defined.
Otherwise, dump a backtrace around the loading call.
If CONTINUE is non-nil, don't stop after first load."
  (and (stringp lib) (setq lib (intern lib)))
  (let* ((lib-name (symbol-name lib))
         (lib-path (or (locate-library lib-name) lib-name))
         (load-syms (mapcar
                     (lambda (s)
                       (cons s (intern (format "%s@watch-%s" s lib))))
                     '(require load)))
         (cleanup (unless continue
                    (cl-loop for (ls . n) in load-syms
                             collect `(advice-remove ',ls ',n))))
         (buf (get-buffer-create (format "*Load Points[%s]*" lib-name))))
    (pcase-dolist (`(,load-sym . ,name) load-syms)
      (advice-add
       load-sym :around
       (defalias `,name
         `(lambda (f sym &rest args)
            (when (or (equal sym ',lib)
                      (and (stringp sym)
                           (or (string= sym ,lib-name)
                               (file-equal-p sym ',lib-path))))
              ,@cleanup
              (let ((standard-output ,buf))
                (if load-in-progress
                    (princ (format "Load: %s => %s\n" ',lib-name load-file-name))
                  (princ "No load in progress:\n")
                  (backtrace))
                (princ "---------------------------------------\n")))
            (apply f sym args)))))
    buf))

;; Usage:
;;  emacs -q -l /this/file.el --eval "(find-initial-load 'tramp)"
(defun find-initial-load (lib &optional no-initialize)
  (let ((buf (find-load-point lib 'continue)))
    (or no-initialize (package-initialize))
    (load my-init-file)
    (pop-to-buffer buf)
    (delete-other-windows)))

;; test that deferred requires still get reported
;; (defun my-f () (require 'subr-x))
;; (add-hook 'emacs-startup-hook #'my-f)
