;;; nvp-shell-macs.el --- shared macros -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(require 'cl-lib)
(require 'nvp-macro)
(require 'macroexp)

(defmacro nvp-shell-goto-command-start (start &optional limit delims)
  "Move point to beginning of current command.
START is the initial point, LIMIT is an optional search bound.
DELIMS are chars that will delimit commands and won't be skipped outside of
strings."
  (or delims (setq delims "^)(|&\`;\["))
  (macroexp-let2 nil limit limit
    `(let (ppss done)
       (while (and (not done) ,(if limit `(> (point) ,limit) t))
         (skip-chars-backward ,delims ,(or limit '(line-beginning-position)))
         (if (eq (char-before) ?\))
             (forward-sexp -1)         ; jump back over a possible subshell
           (setq ppss (parse-partial-sexp ,(or limit '(point-min)) (point)))
           (cond
            ;; presumably reached the beginning of a command
            ((or (not (nth 3 ppss))
                 (eq (char-before) ?\`)
                 (and (eq (char-before) ?\()
                      (eq (char-before (1- (point))) ?$)))
             (setq done t))
            ;; move backward out of enclosing string that shouldn't be a quoted
            ;; command
            (t (up-list -1 t t)))))
       (skip-syntax-forward " " ,start))))

(provide 'nvp-shell-macs)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-shell-macs.el ends here
