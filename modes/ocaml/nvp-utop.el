;;; nvp-utop.el --- ocaml REPL -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; - utop-mode syntax
;; - navigate b/w prompts - like comint-next/previous-prompt
;; - switch b/w source and REPL
;; - various REPL commands
;; - eval phrase from source buffer in utop
;; - redirection form utop process output
;; - load/track utop history to provide hippie expansion
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-repl)
(require 'nvp-hippie-history)
(require 'utop nil t)
(require 'tuareg nil t)
(nvp:decls :p (utop tuareg) :v (nvp-trace-group-alist nvp-ocaml--etc))

;; debugging
(with-eval-after-load 'nvp-trace
  (cl-pushnew '(utop utop-process-output
                     comint-redirect-cleanup
                     nvp-utop-redirect-filter)
              nvp-trace-group-alist
              :test #'equal))

;;; REPL

(defun nvp-utop-init (&optional _prefix)
  (utop-prepare-for-eval)
  (get-buffer-process utop-buffer-name))

(defun nvp-utop-change-directory ()
  (interactive)
  (let ((dir (if (eq major-mode 'utop-mode)
                 (read-directory-name "Change directory to: ")
               default-directory)))
    (utop-eval-string (format "#cd \"%s\";;" dir))))

;; Eval commands
(defun nvp-utop-eval-phrase (&optional arg step)
  (interactive "P")
  (if arg (tuareg-eval-phrase)
    (utop-prepare-for-eval)
    (-let (((beg end _end-with-cmts) (funcall utop-discover-phrase)))
      (nvp-indicate-pulse-region-or-line beg end)
      (utop-eval beg end)
      (when step
        (goto-char end)
        (forward-line 1)))))

(defun nvp-utop-eval-phrase-and-step (&optional arg)
  (interactive "P")
  (funcall-interactively 'nvp-utop-eval-phrase arg 'step))

(nvp-repl-add '(tuareg-mode utop-mode ocaml-ts-mode caml-mode)
  :name 'utop
  :modes '(utop-mode)
  :find-fn (lambda () (ignore-errors (get-buffer utop-buffer-name)))
  :init #'nvp-utop-init
  :send-input #'utop-eval-input
  :send-string #'utop-eval-string
  :send-statement #'utop-eval-phrase
  :send-buffer #'utop-eval-buffer
  :send-region #'utop-eval-region
  :clear-buffer #'ignore
  :help-cmd '(:no-arg "#help;;" :with-arg "#show %s;;")
  :cd-cmd "#cd \"%s\";;"
  :pwd-cmd "#pwd;;")

;;; Syntax
;; default syntax (fundamental) breaks motions
;; . needs to be "." (punctuation) since utop's company-prefix is
;; found with company-grab-symbol-cons "\\.", which creates a
;; a cons of (<module> . prefix), where "." can't have symbol syntax
;; otherwise, when candidate is inserted, the module is erased...
(with-eval-after-load 'tuareg
  (setq utop-mode-syntax-table (copy-syntax-table tuareg-mode-syntax-table)))

(defvar nvp-utop-prompt-regexp "utop[\\[0-9\\]+]> ")

;; drop-in replacements for `comint-next-prompt'/...
(defun nvp-utop-next-prompt (n)
  (interactive "p")
  (let ((paragraph-start nvp-utop-prompt-regexp))
    (end-of-line (if (> n 0) 1 0))
    (forward-paragraph n)
    (if (and (looking-at nvp-utop-prompt-regexp)
             (<= (match-end 0) (line-end-position)))
        (goto-char (match-end 0)))))

(defun nvp-utop-previous-prompt (n)
  (interactive "p")
  (nvp-utop-next-prompt (- n)))

(defun nvp-utop-return ()
  (interactive)
  (goto-char (line-end-position))
  (insert ";;")
  (call-interactively 'utop-eval-input-or-newline))


;;; Utop Process

;; store unfinished output
(defvar nvp-utop-output "")

(defun nvp-utop-run-once (func where callback &optional state &rest args)
  "Execute CALLBACK applied to ARGS once before or after FUNC is called.
WHERE can be :before or :after and is passed to `advice-add'. Optionally,
if STATE specifies one of `utop-state''s, the CALLBACK will run once after
both FUNC is called and `utop-state' is set to STATE."
  (advice-add func where
              (progn
                (let ((name (intern (concat (symbol-name callback) "@once"))))
                  (defalias `,name
                    `(lambda (&rest _)
                       (,@(if (and state (eq func 'utop-set-state))
                              `(when (eq ',state utop-state)) '(progn))
                        (advice-remove ',func ',name)
                        ,(if (and state (not (eq func 'utop-set-state)))
                             `(nvp-utop-run-once
                               'utop-set-state :after ',callback ',state ,@args)
                           `(funcall ',callback ,@args)))))
                  name))))

;; see `utop-process-output' and `utop-process-line'
;; called from `comint-redirect-filter'
(defun nvp-utop-redirect-filter (output)
  "Process utop output to insert in redirect buffer."
  ;; add unprocessed output
  (setq nvp-utop-output (concat utop-output output))
  (let ((lines (split-string output "\n" t " \n"))
        res)
    (save-match-data
      (while (>= (length lines) 2)
        (let ((line (car lines)))
          (if (string-match "\\`\\([a-z-]*\\):\\(.*\\)\\'" line)
              (let ((command (match-string 1 line))
                    (argument (match-string 2 line)))
                (pcase command
                  ((or "stderr" "stdout") (push argument res))
                  ;; don't output any other responses from utop
                  (_)))))
        (setq lines (cdr lines)))
      (setq nvp-utop-output (car lines)) ;see `utop-process-output'
      (mapconcat 'identity (nreverse res) "\n"))))

(defun nvp-utop-redirect-cleanup ()
  "Restore utop process filter and state."
  (interactive)
  (with-current-buffer utop-buffer-name
    (interrupt-process utop-process)
    (comint-redirect-cleanup)
    (utop-set-state 'edit)))

(defun nvp-utop-redirect-output (command output-buffer echo &optional no-display)
  "Send COMMAND to utop process with output to OUTPUT-BUFFER.
With prefix arg ECHO, echo output in utop buffer. Redirection is handled by
`comint-redirect-send-command-to-process', (which see)."
  (interactive "sCommand: \nBOutput Buffer: \nP")
  (if (get-buffer-process utop-buffer-name)
      (let ((proc (get-buffer-process utop-buffer-name))) ;lexical
        (unless (string-match-p ";;\\'" command)
          (setq command (concat command ";;")))
        (with-current-buffer utop-buffer-name
          ;; could just add this to utop's hook
          (add-hook 'comint-redirect-filter-functions
                    #'nvp-utop-redirect-filter nil t)
          (utop-prepare-for-eval)
          (let ((comint-prompt-regexp "^\\(?:__unused__\\)")
                ;; don't let comint check for prompt regexp in *utop* buffer
                comint-redirect-perform-sanity-check
                ;; comint's attempts to change mode-line fail with utop format
                mode-line-process)
            (comint-redirect-send-command-to-process
             (format "input:\ndata:%s\nend:" command)
             output-buffer proc echo no-display)))
        (while-no-input (while (accept-process-output proc 0.2)))
        (nvp-utop-redirect-cleanup))
    ;; the rest is only necessary to be able to call `nvp-utop-redirect-output'
    ;; before there is an active utop process
    ;; it starts `utop' via normal `utop-prepare-for-eval' sequence
    ;; delaying execution of `nvp-utop-redirect-output' until after both
    ;; `utop-start' is run and `utop-set-state' set's the state to 'edit
    (nvp-utop-run-once
     'utop-start :after
     #'nvp-utop-redirect-output 'edit command output-buffer echo no-display)
    (utop-prepare-for-eval)))


;;; History
(defvar nvp-utop-history ())

;; XXX: save / restore instead of reading from utop each time
(defun nvp-utop-load-history ()
  (with-temp-buffer
    (nvp-utop-redirect-output
     (format "#use \"%s\"" (expand-file-name "hist-dump.ml" nvp-ocaml--etc))
     (current-buffer) nil t)
    (erase-buffer)
    (nvp-utop-redirect-output "LTerm_history.dump_lisp()" (current-buffer) nil t)
    (goto-char (point-min))
    (setq nvp-utop-history
          (cl-remove-duplicates
           (read (current-buffer)) :test #'equal :from-end t))))

(define-advice utop-eval-input
    (:around (fn &optional ai ae add-hist im) "add-history")
  (when (and nvp-utop-history add-hist)
    (push (buffer-substring-no-properties utop-prompt-max (point-max))
          nvp-utop-history))
  (funcall fn ai ae add-hist im))

(defun nvp-utop-hippie-setup ()
  (interactive)
  (unless nvp-utop-history
    (nvp-utop-run-once 'hippie-expand :before #'nvp-utop-load-history))
  (with-current-buffer utop-buffer-name
    (setq nvp-he-history 'nvp-utop-history
          nvp-he-history-bol-fn (lambda () utop-prompt-max)))
  (add-to-list 'hippie-expand-try-functions-list #'nvp-he-try-expand-history))

(provide 'nvp-utop)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-utop.el ends here
