;;; nvp-lua-repl.el --- Lua REPL -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Defines major mode for Lua repl, `inf-lua-mode', with font-locking.
;; FIXME: use `comint-indirect-setup-function' for font-locking
;; #<marker at 28479 in shell.el.gz>
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-repl)
(require 'lua-ts-mode nil t)
(require 'comint)
(nvp:decls :p (lua))


(defun nvp-lua-repl-init (&optional prefix)
  "Launch lua repl.
With single PREFIX arg setup for debugger:
 - doesnt load init file
 - enables compilation minor mode
With two \\[universal-argument] prompt for lua command."
  (let ((process-environment           ; Linenoise is useless in emacs
         (append (list "DBG_NOREADLINE=1")
                 (copy-sequence process-environment))))
    (funcall #'inf-lua-run (equal '(16) prefix) nil
             (and nvp-repl-load-startup-file
                  (or (null prefix) (>= (prefix-numeric-value prefix) 16))
                  lua-ts-inferior-startfile))))

(nvp-repl-add '(lua-mode lua-ts-mode)
  :name 'lua
  :modes '(inf-lua-mode)
  :bufname (regexp-quote lua-ts-inferior-buffer)
  :history-file ".lua_history"
  :cd-cmd "lfs=require 'lfs'; lfs.chdir(\"%s\")"
  :pwd-cmd "lfs=require 'lfs'; print(lfs.currentdir())"
  :help-cmd "_G"
  :eval-filter (lambda (s) (replace-regexp-in-string lua-ts-inferior-prompt-continue "" s))
  :init #'nvp-lua-repl-init)

;; -------------------------------------------------------------------
;;; Repl Mode

(defvar inf-lua-repl-compilation-regexp-alist
  '(;; debugger.lua
    ("^[>]?\\s-*break via dbg.+=> \\([^:]+\\):\\([0-9]+\\)" 1 2)
    ("^\\s-*[0-9]+\\(?: =>\\)?\\s-*\\([^:]+\\):\\([0-9]+\\)" 1 2)
    ;; `lua-traceback-line-re'
    ("^\\(?:[\t ]*\\|.*>[\t ]+\\)\\(?:[^\n\t ]+:[0-9]+:[\t ]*\\)*\\(?:\\([^\n\t ]+\\):\\([0-9]+\\):\\)" 1 2)))

(defun inf-lua-calculate-command (&optional prompt default)
  (unless default
    (setq default (concat lua-ts-inferior-program " "
                          (mapconcat 'identity lua-ts-inferior-options " "))))
  (if prompt (read-shell-command "Run Lua: " default)
    default))

;;;###autoload
(defun inf-lua-run (&optional prompt cmd startfile show)
  "Run a Lua interpreter in an inferior process."
  (interactive (list current-prefix-arg nil nil t))
  (let* ((cmd (inf-lua-calculate-command prompt cmd))
         (buffer (inf-lua-make-comint
                  cmd "Lua" (or startfile lua-ts-inferior-startfile) show)))
    (get-buffer-process buffer)))

(defun inf-lua-make-comint (cmd proc-name &optional startfile show)
  (let ((proc-buff-name (format "*%s*" proc-name)))
    (unless (comint-check-proc proc-buff-name)
      (let* ((cmdlist (split-string-and-unquote cmd))
             (program (car cmdlist))
             (args (cdr cmdlist))
             (buffer (apply #'make-comint-in-buffer proc-name
                            proc-buff-name
                            program
                            startfile
                            args)))
        (when lua-ts-inferior-history
          (set-process-sentinel
           (get-buffer-process buffer) #'lua-ts-inferior--write-history))
        (with-current-buffer buffer
          (inf-lua-mode))))
    (when show
      (pop-to-buffer proc-buff-name))
    proc-buff-name))


;; Defined in `lua-ts-inferior-lua'
(defun inf-lua--preoutput-filter (string)
  (if (or (not (equal (buffer-name) lua-ts-inferior-buffer))
          (equal string (concat lua-ts-inferior-prompt-continue " ")))
      string
    (concat
     ;; Filter out the extra prompt characters that
     ;; accumulate in the output when sending regions
     ;; to the inferior process.
     (replace-regexp-in-string (rx-to-string
                                `(: bol
                                    (* ,lua-ts-inferior-prompt
                                       (? ,lua-ts-inferior-prompt)
                                       (1+ space))
                                    (group (* nonl))))
                               "\\1" string)
     ;; Re-add the prompt for the next line.
     lua-ts-inferior-prompt " ")))

;;; Font-lock
;; Stolen from `python.el'

(defcustom inf-lua-font-lock-enable t
  "Non-nil to enable font-locking in the repl buffer."
  :group 'inf-lua
  :type 'boolean)

(defvar-local inf-lua--font-lock-buffer nil)

(defmacro inf-lua-with-shell-buffer (&rest body)
  "Execute the forms in BODY with the shell buffer temporarily current.
Signals an error if no shell buffer is available for current buffer."
  (declare (indent 0) (debug t))
  (let ((shell-process (make-symbol "shell-process")))
    `(let ((,shell-process (get-buffer-process lua-ts-inferior-buffer)))
       (unless ,shell-process
         (user-error "Start a Lua process first"))
       (with-current-buffer (process-buffer ,shell-process)
         ,@body))))

(defmacro inf-lua-font-lock-with-font-lock-buffer (&rest body)
  "Execute the forms in BODY in the font-lock buffer.
The value returned is the value of the last form in BODY.  See
also `with-current-buffer'."
  (declare (indent 0) (debug t))
  `(inf-lua-with-shell-buffer
     (save-current-buffer
       (when (not (and inf-lua--font-lock-buffer
                       (get-buffer inf-lua--font-lock-buffer)))
         (setq inf-lua--font-lock-buffer
               (inf-lua-font-lock-get-or-create-buffer)))
       (set-buffer inf-lua--font-lock-buffer)
       (when (not font-lock-mode)
         (font-lock-mode 1))
       (setq-local delay-mode-hooks t)
       (when (not (derived-mode-p 'lua-ts-mode))
         (lua-ts-mode))
       ,@body)))

(defun inf-lua-font-lock-kill-buffer ()
  "Kill the font-lock buffer safely."
  (when (and inf-lua--font-lock-buffer
             (buffer-live-p inf-lua--font-lock-buffer))
    (kill-buffer inf-lua--font-lock-buffer)
    (when (derived-mode-p 'inf-lua-mode)
      (setq inf-lua--font-lock-buffer nil))))

(defun inf-lua-font-lock-get-or-create-buffer ()
  "Get or create a font-lock buffer for current inferior process."
  (inf-lua-with-shell-buffer
    (if inf-lua--font-lock-buffer
        inf-lua--font-lock-buffer
      (let ((process-name
             (process-name (get-buffer-process (current-buffer)))))
        (generate-new-buffer
         (format " *%s-font-lock*" process-name))))))

(defun inf-lua-font-lock-post-command-hook ()
  "Fontifies current line in inferior lua buffer."
  (let ((prompt-end (cdr comint-last-prompt)))
    (when (and prompt-end (> (point) prompt-end)
               (process-live-p (get-buffer-process (current-buffer))))
      (let* ((input (buffer-substring-no-properties
                     prompt-end (point-max)))
             (deactivate-mark nil)
             (start-pos prompt-end)
             (buffer-undo-list t)
             (replacement
              (inf-lua-font-lock-with-font-lock-buffer
                (delete-region (point-min) (point-max))
                (insert input)
                (font-lock-ensure)
                (buffer-string)))
             (replacement-length (length replacement))
             (i 0))
        ;; Inject text properties to get input fontified.
        (while (not (= i replacement-length))
          (let* ((plist (text-properties-at i replacement))
                 (next-change (or (next-property-change i replacement)
                                  replacement-length))
                 (plist (let ((face (plist-get plist 'face)))
                          (if (not face)
                              plist
                            ;; Replace FACE text properties with
                            ;; FONT-LOCK-FACE so input is fontified.
                            (plist-put plist 'face nil)
                            (plist-put plist 'font-lock-face face)))))
            (set-text-properties
             (+ start-pos i) (+ start-pos next-change) plist)
            (setq i next-change)))))))

(defun inf-lua-font-lock-cleanup-buffer ()
  "Cleanup the font-lock buffer.
Provided as a command because this might be handy if something
goes wrong and syntax highlighting in the shell gets messed up."
  (interactive)
  (inf-lua-with-shell-buffer
    (inf-lua-font-lock-with-font-lock-buffer
      (erase-buffer))))

(defun inf-lua-comint-end-of-output-p (output)
  "Return non-nil if OUTPUT ends with input prompt."
  (string-match
   ;; XXX: It seems on macOS an extra carriage return is attached
   ;; at the end of output, this handles that too.
   (concat
    "\r?\n?"
    ;; Remove initial caret from calculated regexp
    (replace-regexp-in-string
     (rx string-start ?^) ""
     lua-ts-inferior-prompt)
    (rx eos))
   output))

(defun inf-lua-font-lock-comint-output-filter-function (output)
  "Clean up the font-lock buffer after any OUTPUT."
  (unless (string= output "")
    (if (let ((output (ansi-color-filter-apply output)))
          (and (inf-lua-comint-end-of-output-p output)
               (not (string-match lua-ts-inferior-prompt-continue output))))
        ;; If output ends with an initial (not continuation) input prompt
        ;; then the font-lock buffer must be cleaned up.
        (inf-lua-font-lock-cleanup-buffer)
      ;; Otherwise just add a newline.
      (inf-lua-font-lock-with-font-lock-buffer
        (goto-char (point-max))
        (newline)))
    output))

(defun inf-lua-font-lock-turn-on (&optional msg)
  (interactive "p")
  (inf-lua-with-shell-buffer
    (inf-lua-font-lock-kill-buffer)
    (setq-local inf-lua--font-lock-buffer nil)
    (add-hook 'post-command-hook
              #'inf-lua-font-lock-post-command-hook nil t)
    (add-hook 'kill-buffer-hook #'inf-lua-font-lock-kill-buffer nil t)
    (add-hook 'comint-output-filter-functions
              #'inf-lua-font-lock-comint-output-filter-function 'append))
  (when msg
    (message "Inf lua font-lock enabled")))

(define-derived-mode inf-lua-mode comint-mode "Lua"
  "Major mode for lua repl."
  (setq-local comment-start "--"
              comment-end ""
              comment-start-skip "--+ *"
              parse-sexp-ignore-comments t
              parse-sexp-lookup-properties t)
  (setq-local comint-input-ignoredups t
              comint-input-ring-file-name lua-ts-inferior-history
              comint-prompt-read-only t
              comint-prompt-regexp (rx-to-string
                                    `(: bol ,lua-ts-inferior-prompt
                                        (1+ space)))
              comint-output-filter-functions '(ansi-color-process-output
                                               comint-watch-for-password-prompt)
              comint-highlight-input nil)
  (setq-local mode-line-process '(":%s"))

  (add-hook 'comint-preoutput-filter-functions #'inf-lua--preoutput-filter nil t)  
  (when inf-lua-font-lock-enable
    (inf-lua-font-lock-turn-on))
  
  ;; compilation
  (setq-local compilation-error-regexp-alist inf-lua-repl-compilation-regexp-alist)
  (compilation-shell-minor-mode t)
  (comint-read-input-ring t))

(provide 'nvp-lua-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-lua-repl.el ends here
