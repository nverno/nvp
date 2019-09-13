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
;; filter: utop-output-filter
;; sentinel: utop-sentinel

(defvar nvp-utop-redirect-buffer nil)
(defvar nvp-utop-trace
  '(utop-set-state utop-))

;; return live utop process, creating if necessary
(defsubst nvp-utop-process ()
  (let ((proc (get-buffer-process utop-buffer-name)))
    (if (process-live-p proc) proc
      (get-buffer-process (call-interactively #'utop)))))

(defun nvp-utop-redirect-filter (_old-filt _proc line)
  (with-current-buffer nvp-utop-redirect-buffer
    (string-match "\\`\\([a-z-]*\\):\\(.*\\)\\'" line)
    (let ((command (match-string 1 line))
          (argument (match-string 2 line)))
      (message "%s" command)
      (pcase command
        ("stderr" (insert argument "\n"))
        ("stdout" (insert argument "\n"))
        ("prompt" (signal 'nvp-utop-redirect-finished nil))
        (_)))))

(defun nvp-utop-redirect-output (cmd &optional output-buffer no-display)
  (let* ((proc (nvp-utop-process))
         (_orig-buf (process-buffer proc)))
    (setq nvp-utop-redirect-buffer
          (get-buffer-create (or output-buffer "*utop-redirect*")))
    ;; XXX: check state is open first
    ;; (utop-set-state 'wait)
    (unwind-protect
        (progn
          (set-process-buffer proc nvp-utop-redirect-buffer)
          (add-function :around (process-filter proc) #'nvp-utop-redirect-filter)
          (process-send-string proc (format "input:\ndata:%s\nend:\n" cmd))
          ;; (condition-case nil
          ;;     (while (accept-process-output proc 0.5))
          ;;   (nvp-utop-redirect-finished))
          )
      ;; restore process
      ;; (set-process-buffer proc orig-buf)
      ;; (remove-function (process-filter proc) #'nvp-utop-redirect-filter)
      )
    (or no-display
        (display-buffer (get-buffer-create nvp-utop-redirect-buffer)))
    ;; (utop-set-state 'edit)
    ))

(provide 'nvp-utop)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-utop.el ends here
