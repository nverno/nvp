;;; nvp-complete.el --- random completion -*- lexical-binding: t; -*-

;;; Commentary:

;; FIXME:
;; - cleanup/generalize command-switch completion to be usable
;;
;; TODO:
;; - readline completions: how to get in shell-mode?
;; - gud completions have complete cmd - #<marker at 32128 in gud.el.gz>

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'info-look))
(nvp-decl info-lookup-quick-all-modes info-lookup-guess-default*
  info-lookup->completions)

;; -------------------------------------------------------------------
;;; Info

;;;###autoload
(defun nvp-info-completion-at-point ()
  (require 'info-look)
  (-when-let* ((modes (info-lookup-quick-all-modes 'symbol major-mode)))
    (let ((start (point)) try mode)
      (while (and (not try) modes)
        (setq mode (car modes)
              modes (cdr modes)
              try (info-lookup-guess-default* 'symbol mode))
        (goto-char start))
      (when try
        (-when-let (completions (info-lookup->completions 'symbol mode))
          (when (info-lookup->ignore-case 'symbol mode)
            (setq completions
                  (lambda (string pred action)
                    (let ((completion-ignore-case t))
                      (complete-with-action
                       action completions string pred)))))
          (save-excursion
            (goto-char start)
            (while (and (search-backward try nil t)
                        (< start (point))))
            (list (match-beginning 0) (match-end 0) completions :exclusive 'no)))))))

;; -------------------------------------------------------------------
;;; Command line switches 
;; Completion for command line arguments

;; minibuffer history
(defvar nvp-complete-switch-history ())

;; bind dynamically
(defvar-local nvp-complete--switch-program nil)
(defvar-local nvp-complete--switch-args '("--help"))

;; gather command line switches for completion
(nvp-define-cache nvp-complete--switches ()
  "Local command line switches."
  :local t
  (with-temp-buffer
    (apply 'call-process nvp-complete--switch-program
           nil (current-buffer) nil nvp-complete--switch-args)
    (goto-char (point-min))
    (let (res)
      (while (re-search-forward "^\\s-*\\(-[a-zA-Z0-9]+\\)" nil t)
        (push (match-string 1) res)
        (while (re-search-forward "\\(-[a-zA-Z0-9]+\\)" (line-end-position) t)
          (push (match-string 1) res)))
      (nreverse res))))

;; completion at point for command line switch
(defun nvp-complete-switch-completion-at-point ()
  (let ((bnds (bounds-of-thing-at-point 'symbol)))
    (if (and bnds (eq ?- (char-after (car bnds))))
        (list (car bnds) (cdr bnds) (nvp-complete--switches)))))

;;;###autoload
(defun nvp-complete-read-switch (&optional prompt initial-contents)
  "Read from minibuffer with completion for command line switches."
  (or prompt (setq prompt "Command switches: "))
  (let ((minibuffer-completing-symbol nil))
    (minibuffer-with-setup-hook
     (lambda ()
       (add-hook 'completion-at-point-functions
                 'nvp-complete-switch-completion-at-point nil 'local))
     (read-from-minibuffer prompt initial-contents
                           read-expression-map nil
                           'nvp-complete-switch-history))))

;; -------------------------------------------------------------------
;;; Setup macros 

(defmacro nvp-with-switch-completion (program &optional args &rest body)
  "Evaluate BODY with local PROGRAM and ARGS bound."
  (declare (indent defun) (debug (sexp sexp &rest form)))
  `(progn
     (eval-when-compile (require 'nvp-complete))
     (let ((nvp-complete--switch-program ,program)
           (nvp-complete--switch-args ,args))
       ,@body)))

(defmacro nvp-compile-with-switch-completion (program &optional args cmd prompt
                                                      &rest body)
  "Setup `compile-command' with completions for compiler switches.
Execute BODY and `compile'."
  (declare (indent defun) (debug (sexp sexp sexp sexp &rest form)))
  (macroexp-let2 nil program program
   `(nvp-with-switch-completion ,program ,args
      (let ((compile-command
             (format
              (concat ,(or cmd program) " %s %s")
              (nvp-complete-read-switch ,prompt)
              buffer-file-name))
            (compilation-read-command))
        ,@body
        (call-interactively 'compile)))))

(defmacro nvp-complete-compile (program default
                                        &optional args cmd prompt
                                        &rest body)
  "Defines body of compile command. With prefix, prompts for additional
arguments, providing completion for command line switches (determined
from PROGRAM ARGS)."
  (declare (indent defun))
  `(if current-prefix-arg
       (nvp-compile-with-switch-completion ,program ,args ,cmd ,prompt
         ,@body)
     (let ((compile-command (format ,default buffer-file-name))
           (compilation-read-command))
       (call-interactively 'compile))))

(provide 'nvp-complete)
;;; nvp-complete.el ends here
