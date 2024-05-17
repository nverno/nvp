;;; nvp-compile.el --- compile autoloads -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; XXX: integrate with projectile
;;
;; packages:
;; - https://github.com/ReanGD/emacs-multi-compile
;; - https://github.com/defunkt/emacs/blob/master/vendor/mode-compile.el
;; - smart-compile
;; - https://github.com/syohex/emacs-quickrun
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'compile)
(nvp:decls :p (comint xterm) :f (nvp-read-switch nvp-buffer-local-set-key))
(nvp:auto "ansi-color" 'ansi-color-apply-on-region)
(nvp:auto "projectile" 'projectile-project-root)

(defmacro nvp-with-compile-command (cmd &optional arg &rest body)
  "Bind `compile-command' to CMD unless ARG, if non-nil, or `compile-command'
has a file or directory local binding."
  (declare (indent 2))
  `(let ((compile-command
          (or ,@(if arg '(arg))
              (cdr (assoc 'compile-command (buffer-local-variables)))
              ,cmd)))
     ,@body))

;; Create compile function, check for makefiles/cmake first, otherwise
;; execute BODY. Prefix argument executes PROMPT-ACTION, and its
;; result is bound to ARGS, which can be used in the body.
(cl-defmacro nvp-make-or-compile-fn
    (name
     (&key
      (doc "Compile using make or cmake if found, otherwise execute body.")
      (make-action
       '(let ((compile-command
               (or args
                   (cdr (assoc 'compile-command file-local-variables-alist))
                   "make -k")))
          (nvp-compile)))
      (cmake-action '(funcall-interactively #'nvp-cmake-compile))
      (default-prompt '(read-from-minibuffer "Compile args: "))
      (prompt-action
       `((cond
          ,@(and cmake-action '((have-cmake)))
          ,@(and make-action
                 '((have-make
                    (format "make %s" (read-from-minibuffer "Make args: ")))))
          (t ,default-prompt)))))
     &rest body)
  "Create compile function that prefers make or cmake.

\(fn NAME ...)"
  (declare (indent defun))
  (let ((fn (if (symbolp name) name (intern name))))
    `(progn
       (defun ,fn (&optional arg)
         ,doc
         (interactive "P")
         (let* (,@(and make-action
                       '((have-make
                          (memq t (mapcar #'file-exists-p
                                          '("Makefile" "makefile" "GNUMakefile"))))))
                ,@(and cmake-action
                       '((have-cmake (file-exists-p "CMakeLists.txt"))))
                (args (and arg ,@(or prompt-action
                                     '((read-from-minibuffer "Compile args: "))))))
           (cond
            ,@(and cmake-action `((have-cmake ,cmake-action)))
            ,@(and make-action `((have-make ,make-action)))
            (t ,@body)))))))

;;;###autoload
(defun nvp-compile (&optional arg compile-fn)
  "Compile using COMPILE-FN with prefix ARG.
If ARG is 3 or more \\[universal-argument] or COMPILE-FN is \\='default use
`compile'. Otherwise, if COMPILE-FN is nil, use the first non-nil of
`nvp-local-compile-function', `nvp-compile-default-function' or
`nvp-compile-default'."
  (interactive "P")
  (setq compile-fn (or (and (eq compile-fn 'default) #'nvp-compile-default)
                       compile-fn
                       (pcase (prefix-numeric-value arg)
                         ((guard (> 16)) #'compile)
                         (_ (or (bound-and-true-p nvp-local-compile-function)
                                (bound-and-true-p nvp-compile-default-function)
                                #'nvp-compile-default)))))
  (setq current-prefix-arg arg)
  (call-interactively compile-fn))

(defun nvp-compile-default (&optional comint read-command)
  "Compile using `compile-command'.
Use comint with 2 or more \\[universal-argument].
Read command with 1 or 3 or more \\[universal-argument]."
  (interactive (let ((arg (prefix-numeric-value current-prefix-arg)))
                 (list (>= arg 16) (or (eq arg 4) (> arg 16)))))
  (setq-local compilation-read-command read-command)
  (when compilation-read-command
    (setq-local compile-command (compilation-read-command (eval compile-command))))
  (funcall-interactively #'compile (if (functionp compile-command)
                                       (funcall compile-command)
                                     (eval compile-command))
                         comint))

;;;###autoload
(defun nvp-compile-with-switches (set-p &optional cmd &rest args)
  "Read compilation command with switch completion for CMD ARGS.
With prefix, SET-P is passed to `nvp-read-switch' to set local values."
  (interactive "P")
  (let ((compile-command
         (apply #'nvp-read-switch "Compile: " (or cmd (eval compile-command))
                cmd set-p args)))
    (funcall-interactively #'nvp-compile)))

;;;###autoload
(defun nvp-compile-with-bindings (&rest bindings)
  "Run basic compile with local BINDINGS in output buffer.
ARGS are passed to `nvp-compile'."
  (let* ((orig-funcs compilation-finish-functions)
         (lmap (make-sparse-keymap))
         (setup-func
          #'(lambda (&rest _ignored)
              (set-keymap-parent lmap (current-local-map))
              (use-local-map lmap)
              (setq compilation-finish-functions orig-funcs))))
    (pcase-dolist (`(,k . ,b) bindings)
      (define-key lmap (if (vectorp k) k (kbd k)) b))
    (setq compilation-finish-functions setup-func)
    (call-interactively 'nvp-compile)))

;;;###autoload
(defun nvp-compile-local (cmd &optional root key)
  "Set `compile-command' to CMD in dir-locals.
Optionally run in ROOT, eg. `(c++-mode . (eval . (nvp-compile-local CMD t)))'."
  (setq-local compile-command
              (concat (if root (concat "cd " (projectile-project-root) " && ")) cmd))
  (nvp-buffer-local-set-key (kbd (or key "C-c C-c")) #'compile))

;; -------------------------------------------------------------------
;;; Related commands

;; TODO: display compiler info
;;;###autoload
(defun nvp-compile-help ()
  (interactive))


;; -------------------------------------------------------------------
;;; Compilation

;;;###autoload
(defun nvp-compile-colorize ()
  (interactive)
  (let ((inhibit-read-only t))
    ;; prefer xterm when available
    (if (fboundp 'xterm-color-colorize-buffer)
        (xterm-color-colorize-buffer)
      (ansi-color-apply-on-region compilation-filter-start (point-max)))))

;; move to next warning/error
(defun nvp-compilation-next (n)
  "Move to next warning, error, or info message."
  (interactive "p")
  (let ((compilation-skip-threshold 0))
    (compilation-next-error n)
    (setq compilation-current-error (point-marker))))

(defun nvp-compilation-previous (n)
  (interactive "p")
  (nvp-compilation-next (- n)))

(defun nvp-next-error-no-select (n)
  (interactive "p")
  (let ((compilation-skip-threshold 0))
    (next-error-no-select n)))

(defun nvp-previous-error-no-select (n)
  (interactive "p")
  (nvp-next-error-no-select (- n)))

;;; Compilation-shell-minor-mode

(defun nvp-compilation-next-or-complete (n)
  "Unless after comint prompt, move to Nth next error, otherwise complete."
  (interactive "p")
  (if (comint-after-pmark-p)
      (completion-at-point)
    (nvp-compilation-next n)))

(provide 'nvp-compile)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-compile.el ends here
