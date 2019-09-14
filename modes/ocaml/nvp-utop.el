;;; nvp-utop.el --- ocaml REPL -*- lexical-binding: t; -*-

;;; Commentary:
;; TODO:
;; - macro to dump utop history
;; - hippie-exp from hist
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'tuareg)
(require 'utop)
(require 'nvp)
(nvp-decls)

;;; Syntax
;; default syntax (fundamental) breaks motions
;; . needs to be "." (punctuation) since utop's company-prefix is
;; found with company-grab-symbol-cons "\\.", which creates a
;; a cons of (<module> . prefix), where "." can't have symbol syntax
;; otherwise, when candidate is inserted, the module is erased...
(setq utop-mode-syntax-table (copy-syntax-table tuareg-mode-syntax-table))

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

;; switch b/w inferior and source buffers
;;;###autoload
(defun nvp-utop-switch-buffers ()
  (interactive)
  (switch-to-buffer-other-window
   (if (eq major-mode 'utop-mode)
       (-if-let (proc (nvp-buffer-process))
           (let ((src-buf (process-get proc :src-buffer)))
             (or (and (buffer-live-p src-buf) src-buf)
                 (other-buffer (current-buffer) 'visible)))
         (other-buffer (current-buffer) 'visible))
     (let ((src (current-buffer)))
       (utop-prepare-for-eval)
       (with-current-buffer (get-buffer utop-buffer-name)
         (process-put (nvp-buffer-process) :src-buffer src)
         (current-buffer))))))

(defun nvp-utop-return ()
  (interactive)
  (goto-char (line-end-position))
  (insert ";;")
  (call-interactively 'utop-eval-input-or-newline))

(defun nvp-utop-change-directory ()
  (interactive)
  (let ((dir (if (eq major-mode 'utop-mode)
                 (read-directory-name "Change directory to: ")
               default-directory)))
    (utop-eval-string (format "#cd \"%s\";;" dir))))

;;; Eval commands
;; (9/7/19) utop-eval-phrase errors in source buffers
(defun nvp-utop-eval-phrase (&optional arg step)
  (interactive "P")
  (if arg (tuareg-eval-phrase)
    (utop-prepare-for-eval)
    (-let (((beg . end) (funcall utop-discover-phrase)))
      (nvp-indicate-pulse-region-or-line beg end)
      (utop-eval beg end)
      (when step
        (goto-char end)
        (forward-line 1)))))

(defun nvp-utop-eval-phrase-and-step (&optional arg)
  (interactive "P")
  (funcall-interactively 'nvp-utop-eval-phrase arg 'step))


;;; Utop Process
(with-eval-after-load 'nvp-trace
  (cl-pushnew '(utop utop-process-output
                     comint-redirect-cleanup
                     nvp-utop-redirect-filter)
              nvp-trace-group-alist))

(defun nvp-utop-run-once (func where callback &optional state &rest args)
  "Execute CALLBACK applied to ARGS once before or after FUNC is called.
WHERE can be :before or :after and is passed to `advice-add'.
Optionally, if STATE specifies one of `utop-state''s, the CALLBACK will run once 
after both FUNC is called and `utop-state' is set to STATE."
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

;; see `utop-process-line'; called from `comint-redirect-filter'
(defun nvp-utop-redirect-filter (lines)
  "Process utop output to insert in redirect buffer."
  (save-match-data
    (mapconcat
     #'identity
     (cl-loop for line in (split-string lines "\n" t " \n")
        if (string-match "\\`\\([a-z-]*\\):\\(.*\\)\\'" line)
        collect
          (let ((command (match-string 1 line))
                (argument (match-string 2 line)))
            (pcase command
              ((or "stderr" "stdout") argument)
              ;; don't output any other responses from utop
              (_))))
     "\n")))

(defun nvp-utop-redirect-cleanup ()
  "Restore utop process filter and state."
  (interactive)
  (with-current-buffer utop-buffer-name
    (interrupt-process utop-process)
    (comint-redirect-cleanup)
    (utop-set-state 'edit)))

(defun nvp-utop-redirect-output (command output-buffer echo &optional no-display)
  "Send COMMAND to utop process with output to OUTPUT-BUFFER.
With prefix arg ECHO, echo output in utop buffer.
Redirection is handled by `comint-redirect-send-command-to-process', (which see)."
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

(provide 'nvp-utop)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-utop.el ends here
