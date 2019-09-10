;;; nvp-inf.el --- inferior processes -*- lexical-binding: t; -*-

;;; Commentary:

;; TODO: cleanup
;; inferior process utilities
;; lots of unused stuff

;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp-declare nvp-inf-read-process)

;; error signals
(put 'nvp-inf-process-died 'error-conditions '(nvp-inf-process-died error))
(put 'nvp-inf-process-busy 'error-conditions '(nvp-inf-process-busy error))
(put 'nvp-inf-process-async 'error-conditions '(nvp-inf-process-async error))
(put 'nvp-inf-process-uninterruptable 'error-conditions
     '(nvp-inf-process-uninterruptable error))
(put 'nvp-inf-process-interrupt-failed 'error-conditions
     '(nvp-inf-process-interrupt-failed error))

(defvar nvp-inf-output-buffer "*nvp-inf*")

;; #<marker at 49601 in ess-utils.el>
(defun nvp--inject-code-from-file (proc file)
  (let ((content (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-string))))
    ;; (when (string= ess-dialect "R")
    ;;   ;; don't detect intermediate prompts
    ;;   (setq content (concat "{" content "}\n")))
    (nvp-inf-redirect-output proc content)))

;; ------------------------------------------------------------
;;; Finding procs

;; find process started by command NAME
(defun nvp-inf-find-process (name)
  (let ((procs (process-list))
        (completion-ignore-case t)
        (case-fold-search t)
        found proc)
    (while (and (not found) procs
                (process-live-p (setq proc (pop procs)))
                (process-command proc))
      (when (string-match-p name
                            (file-name-sans-extension
                             (file-name-nondirectory
                              (car (process-command proc)))))
        (setq found proc)))
    found))

;; non-nil if process NAME is running and alive
(defun nvp-inf-process-running-p (name)
  (cl-loop for proc in (process-list)
     if (and (string= name (process-name proc))
             (process-live-p proc))
     return t))

;; ------------------------------------------------------------
;;; Prompts

(defvar inferior-ess-prompt)
(defvar comint-use-prompt-regexp)
(defvar comint-prompt-regexp)
(defvar shell-prompt-pattern)

;; return the prompt regexp for proc if available
(defvar nvp-inf-prompt nil)
(defun nvp-inf-prompt (proc &optional check)
  (or (and (not check)
           (cdr (assoc-string (process-name proc) nvp-inf-prompt)))
      (let ((prompt (nvp-inf-get-prompt proc)))
        (when prompt
          (cl-pushnew (cons (process-name proc) prompt) nvp-inf-prompt)
          prompt))))

(defun nvp-inf-get-prompt (proc)
  (with-current-buffer (process-buffer proc)
    (cond
     ((derived-mode-p 'shell-mode)
      (and comint-use-prompt-regexp shell-prompt-pattern))
     ((derived-mode-p 'comint-mode)
      (and comint-use-prompt-regexp comint-prompt-regexp))
     ((derived-mode-p 'inferior-ess-mode)
      inferior-ess-prompt)
     (t nil))))

;; ------------------------------------------------------------
;;; Process Attributes

;; For processes with prompt regexp (from ess):
;;
;; Attributes:
;; - busy: set prior to accepting process output
;; - last-eval: timestamp of last time status was set
;; - busy-end?: true when prompt is found
;; - sec-prompt: secondary prompt pattern. If set, proc returns when it
;;   is found, regardless of whether primary prompt is found.
;; - running-async?: true if process is run asynchronously
;;   (not implemented)
;; - interruptable?: non-nil if async has an interrupt callback
;; - callbacks: list of either functions with arguments PROC and STRING,
;;   or cons cell of form (func . suppress), where if SUPPRESS is non-nil
;;   then next output will be suppressed

(defsubst nvp-inf-mark-as-busy (proc)
  (process-put proc 'busy t)
  (process-put proc 'sec-prompt nil))

;; ** Requires a prompt pattern **
;; return the busy state of process, optionally set timestamp
;; state is determined by matching 
(defun nvp-inf-set-status (proc string &optional no-timestamp
                                sec-prompt)
  (let ((busy (not (string-match-p (concat
                                    "\\(" (nvp-inf-prompt proc)
                                    "\\)\\'")
                                 string))))
    (process-put proc 'busy-end? (and (not busy)
                                      (process-get proc 'busy)))
    (when (not busy)
      (process-put proc 'running-async? nil))
    (process-put proc 'busy busy)
    (when sec-prompt
      (process-put proc 'sec-prompt
                   (string-match-p (concat "\\(" sec-prompt "\\)\\'")
                                   string)))
    (unless no-timestamp
      (process-put proc 'last-eval (current-time)))))

;; ------------------------------------------------------------
;;; Accept output / interrupt processes

;; #<marker at 50722 in ess-inf.el>
;; If there is a prompt, wait until process isn't busy by checking
;; for prompt pattern. If sec-prompt, return if secondary prompt is
;; detected. When there is no prompt, just accept process output for WAIT
;; seconds (or 1 by default.
(defun nvp-inf-wait-for-process (proc use-prompt
                                      &optional sec-prompt wait
                                      force-redisplay)
  (cond
   ((not (eq (process-status proc) 'run))
    (signal 'nvp-inf-process-died nil))
   ((process-get proc 'busy)
    (signal 'nvp-inf-process-busy nil))
   ((process-get proc 'async)
    (signal 'nvp-inf-process-async nil))
   (t
    (if use-prompt
        (let ((start-time (float-time))
              (wait (or wait 0.002)))
          (save-excursion
            (while (or (accept-process-output proc wait)
                       (if (and sec-prompt (process-get proc
                                                        'sec-prompt))
                           nil
                         (if force-redisplay
                             (redisplay 'force))
                         (process-get proc 'busy)))
              (if (> (- (float-time) start-time) 0.5)
                  (setq wait 0.5)))))
      (accept-process-output proc (or wait 1))))))

;; interrupt process if it interruptable?
;; reroutes output to `nvp-inf-output-buffer' and calls callbacks on
;; process.
(defun nvp-inf-interrupt-process-maybe (proc &optional out-filter)
  (when (process-get proc 'interruptable?)
    (signal 'nvp-inf-process-uninterruptable nil))
  (let ((cb (cadr (process-get proc 'callbacks)))
        (buf (get-buffer-create nvp-inf-output-buffer))
        (og-filt (process-filter proc))
        (og-buff (process-buffer proc))
        (out-filter (or out-filter
                        ;; if it has a prompt, use prompt filter
                        ;; without timestamping output
                        (and (nvp-inf-prompt proc)
                             'nvp-inf-prompt-ordinary-filter)))
        (use-prompt (nvp-inf-prompt proc)))
    (unwind-protect
        (progn
          (process-put proc 'interruptable? nil)
          (process-put proc 'callbacks nil)
          (process-put proc 'running-async? nil)
          (set-process-buffer proc buf)
          (set-process-filter proc out-filter)
          (interrupt-process proc)
          ;; if there is a callback, call it
          (when cb

            (funcall cb proc))
          (condition-case nil
              (nvp-inf-wait-for-process proc use-prompt)
            (error
             (signal 'nvp-inf-process-interrupt-failed nil))))
      (set-process-buffer proc og-buff)
      (set-process-filter proc og-filt))))

;; ------------------------------------------------------------
;;; Filters
;;
;; - preinput, input, preoutput, output, redirect

;; Set status by prompt regex, run callbacks
;; If suppress-next-output? is nil, pass to `comint-output-filter'
(defun nvp-inf-prompt-output-filter (proc string)
  (nvp-inf-set-status proc string t)
  (nvp-inf-run-callback proc string)
  (if (process-get proc 'suppress-next-output?)
      (process-put proc 'suppress-next-output? nil)
    (comint-output-filter proc string)))

;; doesn't record timestamp
;; #<marker at 51819 in ess-inf.el>
(defun nvp-inf-prompt-ordinary-filter (proc string)
  (nvp-inf-set-status proc string t)
  (nvp-inf-run-callback proc string)
  (with-current-buffer (process-buffer proc)
    (insert string)))

;; ------------------------------------------------------------
;;; Callbacks
;;
;; Do special things with certain strings, ie '?' or ';'
;; callback is stored in 'callbacks proc property. Callbacks is a list
;; that can contain either functions to be called with two arguments
;; PROC and STRING, or cons cell of the form (func . suppress). If
;; SUPPRESS is non-nil then the next process output will be suppressed.
;; #<marker at 19996 in ess-inf.el>
(defun nvp-inf-run-callback (proc string)
  (unless (process-get proc 'busy)
    (let* ((cb (car (process-get proc 'callbacks)))
           (listp (not (functionp cb)))
           (suppress (and listp (consp cb) (cdr cb)))
           (cb (if (and listp (consp cb))
                   (car cb)
                 cb)))
      (when cb
        (when suppress
          (process-put proc 'suppress-next-output? t))
        (process-put proc 'callbacks nil)
        (condition-case err
            (funcall cb proc string)
          (error (message "%s" (error-message-string err))))))))

;; ------------------------------------------------------------
;;; Redirect process output

;; temporarily redirect PROC process output from command CMD to
;; OUT-BUFFER
;; #<marker at 61386 in ess-inf.el>
;;;###autoload
(defun nvp-inf-redirect-output (proc cmd &optional
                                     out-buffer out-filter and-go
                                     wait force-redisplay
                                     no-prompt-check)
  (interactive
   (list (nvp-inf-read-process)
         (format "%s\n" (read-string "Command (adds \"\\n\"): "))
         nvp-inf-output-buffer nil t))
  (if (stringp proc)
      (setq proc (nvp-inf-find-process proc)))
  (when proc
    ;; try to interrupt process
    (condition-case nil
        (nvp-inf-interrupt-process-maybe proc)
      (nvp-inf-process-interrupt-failed
       (error "Process %s interrupt failed" (process-name proc)))
      (nvp-inf-process-uninterruptable
       (error "Can't interrupt process %s, marked as uninterruptable"
              (process-name proc))))
    ;; process was interrupted, store original process params
    (let* ((out-buffer (get-buffer-create (or out-buffer
                                              nvp-inf-output-buffer)))
           (og-buff (process-buffer proc))
           (og-filt (process-filter proc))
           (og-mark (marker-position (process-mark proc)))
           (out-filter (or out-filter
                           ;; if it has a prompt, use prompt filter
                           ;; without timestamping output
                           (and (nvp-inf-prompt proc)
                                'nvp-inf-prompt-ordinary-filter)))
           (use-prompt (nvp-inf-prompt proc)))
      (unwind-protect
          ;; execute redirection
          (progn
            (set-process-buffer proc out-buffer)
            (set-process-filter proc out-filter)
            (with-current-buffer out-buffer
              (let ((inhibit-read-only t))
                (erase-buffer)
                (set-marker (process-mark proc) (point-min))
                (nvp-inf-mark-as-busy proc)
                (process-send-string proc cmd)
                ;; attempt to accept output from process
                (condition-case nil
                    (if use-prompt
                        (if no-prompt-check
                            (sleep-for 0.02)
                          (nvp-inf-wait-for-process proc 'use-prompt
                                                    nil wait
                                                    force-redisplay))
                      (nvp-inf-wait-for-process proc nil nil wait))
                  (nvp-inf-process-died
                   (error "Process %s has died" (process-name proc)))
                  (nvp-inf-process-busy
                   (error "Process %s is busy" (process-name proc)))
                  (nvp-inf-process-async
                   (error "Process %s is running asynchronously"
                          (process-name proc)))
                  (error "Accepting process output from %s failed"
                         (process-name proc))))))
        ;; restore process
        (set-process-buffer proc og-buff)
        (set-process-filter proc og-filt)
        (set-marker (process-mark proc) og-mark og-buff)))
    (when and-go
      (pop-to-buffer out-buffer)
      (goto-char (point-min)))))

;; same as nvp-inf-redirect-output, only do it asynchronously
;; #<marker at 65432 in ess-inf.el>
(defun nvp-inf-async-redirect-output (proc cmd &optional
                                           out-buffer _out-filter
                                           callback interrupt-callback)
  (interactive
   (list (nvp-inf-read-process)
         (format "%s\n" (read-string "Command (adds \"\\n\"): "))
         nvp-inf-output-buffer nil t t))
  (when (stringp proc)
    (setq proc (nvp-inf-find-process proc)))
  ;; check process status
  (cond
   ((not (and proc (eq (process-status proc) 'run)))
    (error "Process %s is dead" (process-name proc)))
   ((process-get proc 'busy)
    (error "Process %s is busy." (process-name proc)))
   ((process-get proc 'running-async?)
    (error "Process %s is running asynchronously."
           (process-name proc)))
   (t
    (when (eq interrupt-callback t)
      (setq interrupt-callback (lambda (_proc))))
    (process-put proc 'callbacks (list (cons callback 'suppress-output)
                                       interrupt-callback))
    (process-put proc 'interruptable? (and interrupt-callback t))
    (process-put proc 'running-async? t)
    (nvp-inf-redirect-output proc cmd out-buffer nil nil 0.01 nil
                             'no-prompt-check))))

(provide 'nvp-inf)
;;; nvp-inf.el ends here
