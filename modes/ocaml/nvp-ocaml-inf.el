;;; nvp-ocaml-inf.el --- ocaml REPL -*- lexical-binding: t; -*-

;;; Commentary:
;; TODO:
;; - macro to dump utop history
;; - company-complete in REPL erases modifier
;; - no capf backend in REPL
;; - hippie-exp from hist
;; - C-c C-d to change dir
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'tuareg)
(require 'utop)
(require 'nvp)
(nvp-decls)

;;; Syntax
;; default syntax breaks motions
(let ((tab utop-mode-syntax-table))
  (dolist (c (string-to-list ";.,_-^:@#%!~"))
    (modify-syntax-entry c "_" tab)))

;;; REPL commands
(nvp-bindings nvp-utop-fast-map nil
  :create t :repeat t :indicate t
  ("n" . nvp-utop-next-prompt)
  ("p" . nvp-utop-previous-prompt))

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
;; (9/7/19) bug in utop-eval-phrase
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

(defun nvp-utop-redirect-output (cmd)
  (utop-set-state 'wait)
  (let ((comint-prompt-regexp "^prompt:")
        comint-redirect-perform-sanity-check
        mode-line-process)
    (comint-redirect-send-command-to-process
     (format "input:\ndata:%s\nend:\n" cmd) "*utop-redirect*" utop-process nil))
  (utop-set-state 'edit))

(provide 'nvp-ocaml-inf)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-ocaml-inf.el ends here
