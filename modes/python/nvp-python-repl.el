;;; nvp-python-repl.el --- Python repl -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; FIXME: to get proper completion in python REPL following chained calls,
;;         `python-shell-completion-at-point' doesn't correctly skip back over
;;         '().', so the function could be advised (similar to modification for
;;         octave in nvp-octave.el
;;
;;; Code:

(eval-when-compile (require 'nvp-macro))
(require 'nvp-repl)
(require 'python)
(nvp:decls :p (conda-env) :f (conda-env-send-buffer))

;; add regexp for errors in code sent to REPL
(defvar nvp-python-compilation-regexp
  '("^\\([^<\n]+\\) in <[^>]+>\n\\(?:\\s-*[0-9]+.*\n\\)*---> \\([0-9]+\\)" 1 2))

(defun nvp-python-repl-init (&optional _prefix)
  (interactive "P")
  (save-window-excursion
    (call-interactively #'conda-env-send-buffer)))

(nvp-repl-add '(python-mode python-ts-mode)
  :name 'python
  :modes '(inferior-python-mode)
  :init #'nvp-python-repl-init
  :find-fn #'python-shell-get-process
  :send-string #'python-shell-send-string
  :eval-string #'python-shell-send-string-no-output
  :send-region #'python-shell-send-region
  :send-statement #'python-shell-send-statement
  :send-file #'python-shell-send-file
  ;:eval-sexp #'nvp-python-eval-last-sexp
  :cd-cmd "import os; os.chdir(\"%s\")"
  :pwd-cmd "import os; os.getcwd()"
  :help-cmd '(:no-arg "help()" :with-arg "help(%s)"))


(defsubst nvp-python--statement-bounds ()
  (cons (python-nav-beginning-of-statement) (python-nav-end-of-statement)))

;;;###autoload
(defun nvp-python-send-line-dwim (&optional arg)
  "Send current statement and step -- current statement is that containing the
point or the statement directly preceding the point if the point isn't at 
the beginning of the line. With prefix, add a print statement and pop to 
the console."  
  (interactive "P")
  (and (python-info-current-line-comment-p)
       (forward-comment (point-max)))
  (let ((bnds (nvp-python--statement-bounds)))
    (when (and (not (bolp))                   ; maybe directly after statement
               (equal (car bnds) (cdr bnds))) ; and not in a statement
      (setq bnds (progn (beginning-of-line)   ; so, try from beginning of line
                        (nvp-python--statement-bounds))))
    (if (and (not arg) (not (equal (car bnds) (cdr bnds))))
        (python-shell-send-region (car bnds) (cdr bnds))
      (save-excursion
        (goto-char (car bnds))
        (if (looking-at-p "\\s-*print")
            (python-shell-send-region (car bnds) (cdr bnds))
          (python-shell-send-string
           (format
            "print(%s)" (buffer-substring-no-properties (car bnds) (cdr bnds))))
          (display-buffer (process-buffer (python-shell-get-process))))))
    (if (eobp)
        (newline-and-indent)
      (forward-line))))

;;;###autoload
(defun nvp-python-eval-last-sexp (&optional arg)
  (interactive "P")
  (let* ((bnds (nvp-python--statement-bounds))
         (res (python-shell-send-string-no-output
               (buffer-substring-no-properties (car bnds) (cdr bnds)))))
    (when res
      (if arg
          (let ((standard-output (current-buffer)))
            (princ res))
        (message "%s" res)))))

(provide 'nvp-python-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-python-repl.el ends here
