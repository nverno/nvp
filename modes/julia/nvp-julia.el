;;; nvp-julia.el --- julia mode utilities -*- lexical-binding: t -*-
;;; Commentary:
;; TODO:
;; - add company jump to location function
;; - fix macrostep-julia
;; - fix capf w/ ess + latexsubs
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls :p (ess julia))

(with-eval-after-load 'ess-julia
  ;; FIXME: `julia-latexsub' still called by ESS
  (unless (fboundp 'julia-latexsub)
    (defun julia-latexsub (&rest _) nil)))

(with-eval-after-load 'nvp-repl
  ;; FIXME: ess stuff messed up running from non-ess source buffer
  (when (fboundp 'run-ess-julia)
    (nvp-repl-add '(julia-mode julia-ts-mode)
      :name 'ess-julia
      :modes '(inferior-ess-julia-mode)
      :bufname (regexp-quote "*julia:bin")
      :init (lambda (&rest _)
              (save-window-excursion (funcall-interactively #'run-ess-julia)))
      :pwd-cmd "pwd()"
      :cd-cmd "cd(\"%s\")"
      :help-cmd '(:no-arg "" :with-arg "? %s"))))

;;; Snippets

;; Split a julia argument string into ((name, default)..) tuples
(defun nvp-julia-split-args (arg-string)
  (and arg-string
       (mapcar (lambda (x)
                 (split-string x "[[:blank:]]*=[[:blank:]]*" t))
               (split-string arg-string "[[:blank:]]*[,;][[:blank:]]*" t))))
 
;; return docstring format for the julia arguments in yas-text
(defun nvp-julia-args-to-docstring ()
  (if-let* ((indent (concat "\n" (make-string (current-column) ? )))
            (args (nvp-julia-split-args (yas-text)))
            (max-len
             (if args
                 (apply 'max (mapcar (lambda (x) (length (nth 0 x))) args))
               0))
            (formatted-args
             (mapconcat
              (lambda (x)
                (concat "- "
                        (nth 0 x)
                        (make-string (- max-len (length (nth 0 x))) ? )
                        " : "
                        (if (nth 1 x)
                            (concat "\(default " (nth 1 x) "\)"))))
              args
              indent)))
      (if (string-empty-p formatted-args) ""
        (mapconcat 'identity (list "# Arguments" formatted-args) indent))
    ""))

(provide 'nvp-julia)
;;; nvp-julia.el ends here
