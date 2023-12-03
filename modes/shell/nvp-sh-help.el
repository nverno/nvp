;;; nvp-sh-help.el --- help-at-point for sh-script -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; - Within '[' or '[[' => show help for switches
;; - On shell builtin   => bash -c 'help %s'
;; - Otherwise          => use man
;;
;; TODO:
;; - ${} formatting
;; - special variables
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-shell-common)
(require 'nvp-man) ;; parse 'man' stuff

;; ignore ':', not symbolized to match strings
(eval-and-compile
  (defconst nvp-sh--bash-builtins
    (rx (or "." "[" "[[" "alias" "bg" "bind" "break" "builtin" "case" "cd"
            "command" "compgen" "complete" "continue" "declare" "dirs"
            "disown" "echo" "enable" "eval" "exec" "exit" "export" "fc" "fg"
            "getopts" "hash" "help" "history" "if" "jobs" "kill" "let"
            "local" "logout" "mapfile" "popd" "printf" "pushd" "pwd" "read"
            "readarray" "readonly"
            "return" "set" "shift" "shopt" "source" "suspend" "test" "times"
            "trap" "type" "typeset" "ulimit" "umask" "unalias" "unset"
            "until" "wait" "while")
        eos)))

;; get conditional switch
(defun nvp-sh--conditional-switch ()
  (save-excursion
    (skip-chars-backward "^\[" (line-beginning-position))
    (and (not (bolp))
         (looking-at "[ !]*\\(-[[:alpha:]]+\\)")
         (match-string 1))))

(defsubst nvp-sh--bash-builtin-p (cmd)
  (string-match-p nvp-sh--bash-builtins (regexp-quote cmd)))

;; synopsis: bash -c 'help -s %s'
;; help:     bash -c 'help %s'
(defsubst nvp-sh-bash-builtin-help-sync (cmd &optional synopsis)
  (shell-command-to-string
   (concat "bash -c 'help " (and synopsis "-s ") cmd "'")))

(defun nvp-sh-bash-builtin-help (cmd &optional sync buffer)
  (let ((cmd (concat "bash -c 'help " cmd "'"))
        (buffer (or buffer "*sh-help*")))
    (if sync
        (call-process-shell-command cmd nil buffer)
      (start-process-shell-command "bash" buffer cmd))))

;; output to BUFFER, return process
;; man --names-only %s | col -b
(defun nvp-sh-man-help (cmd &optional sync buffer)
  (let ((cmd (concat "man --names-only " (regexp-quote cmd) " | col -b")))
    (if sync
        (call-process-shell-command cmd nil (or buffer "*sh-help*"))
      (start-process-shell-command "man" (or buffer "*sh-help*") cmd))))

;; do BODY in buffer with man output
(defmacro nvp-sh:with-man-help (cmd &optional sync &rest body)
  (declare (indent defun))
  (let ((buffer (make-symbol "temp-buffer")))
    `(let ((,buffer (generate-new-buffer " *sh-help*")))
       ,(if sync
            `(with-current-buffer ,buffer
               (unwind-protect
                   (progn
                     (nvp-sh-man-help ,cmd 'sync (current-buffer))
                     ,@body)
                 (and (buffer-name ,buffer) (kill-buffer ,buffer))))
          `(set-process-sentinel
            (nvp-sh-man-help ,cmd nil ,buffer)
            #'(lambda (p _m)
                (when (zerop (process-exit-status p))
                  (with-current-buffer ,buffer
                    (unwind-protect (progn ,@body)
                      (and (buffer-name ,buffer) (kill-buffer ,buffer)))))))))))

;; if bash builtin do BASH else MAN
(defmacro nvp-sh:with-bash/man (cmd bash &rest man)
  (declare (indent 1) (indent 2))
  `(if (nvp-sh--bash-builtin-p ,cmd)
       ,bash
     ,@man))

(eval-when-compile
  ;; process BODY in output of help for CMD. Help output is
  ;; either from 'bash help' for bash builtins or 'man'.
  (defmacro nvp:sh-with-help (command &optional sync &rest body)
    (declare (indent defun) (debug t))
    (nvp:with-syms (buffer cmd)
      `(let ((,buffer (generate-new-buffer " *sh-help*"))
             (,cmd ,command))
         (if ,sync
             (with-current-buffer ,buffer
               (unwind-protect
                   (progn
                     (apply (nvp-sh:with-bash/man ,cmd
                                #'nvp-sh-bash-builtin-help
                              #'nvp-sh-man-help)
                            ,cmd 'sync (list ,buffer))
                     ,@body)
                 (and (buffer-name ,buffer) (kill-buffer ,buffer))))
           (set-process-sentinel
            (apply (nvp-sh:with-bash/man
                       ,cmd #'nvp-sh-bash-builtin-help #'nvp-sh-man-help)
                   ,cmd nil (list ,buffer))
            #'(lambda (p _m)
                (when (zerop (process-exit-status p))
                  (with-current-buffer ,buffer
                    (unwind-protect (progn ,@body)
                      (and (buffer-name ,buffer) (kill-buffer ,buffer))))))))))))


;;; Parse output

(defsubst nvp-sh--builtin-string ()
  (goto-char (point-min))
  ;; skip synopsis
  ;; (forward-line 1)
  (buffer-substring (point) (point-max)))

(defsubst nvp-sh--man-string (&optional section)
  (setq section (if section (concat "^" section) "^DESCRIPTION"))
  (nvp-help-man-string section))

;; parse 'man bash' conditional switches for '[[' and '['
(defsubst nvp-sh--cond-switches ()
  (nvp-help-man-switches "^CONDITIONAL EXP" "\\s-+-" "^[[:alpha:]]"))

;;; Cache / Lookup
(defvar nvp-sh--doc-cache (make-hash-table :test 'equal))
(defvar nvp-sh--conditional-cache (make-hash-table :test 'equal))

;; return help string for CMD synchronously, cache result
(defun nvp-sh--function-string (cmd &optional section recache)
  (or (and (not recache)
           (gethash cmd nvp-sh--doc-cache))
      (nvp:sh-with-help cmd 'sync
        (let ((res (nvp-sh:with-bash/man cmd
                       (nvp-sh--builtin-string)
                     (nvp-sh--man-string section))))
          (puthash cmd res nvp-sh--doc-cache)
          res))))

;; return conditional help string
(defun nvp-sh--conditional-string (switch)
  (or (gethash switch nvp-sh--conditional-cache)
      (gethash "all" nvp-sh--conditional-cache)
      (progn
        (nvp-sh--cache-conditionals)
        (nvp-sh--conditional-string switch))))

;; make nvp-sh--conditional-cache
(defun nvp-sh--cache-conditionals ()
  (nvp:sh-with-help "bash" 'sync
    (let ((entries (nvp-sh--cond-switches)))
      (dolist (entry entries)
        ;; key by -%s if possible
        (if (string-match "^\\(-[[:alnum:]]+\\).*" (car entry))
            (puthash (match-string 1 (car entry))
                     (concat (car entry) "\n" (cdr entry))
                     nvp-sh--conditional-cache)
          ;; otherwise just use entry, for things like "string = string" from
          ;; man
          (puthash (car entry) (cdr entry) nvp-sh--conditional-cache)))
      ;; make one big entry for everything
      (puthash "all" (mapconcat #'(lambda (entry) (concat (car entry) "\n" (cdr entry)))
                                entries
                                "\n")
               nvp-sh--conditional-cache))))

;;; Man sections for completing read. Not optimal, currently calls
;; man twice - once to get completions, and again to get popup.
(defsubst nvp-sh--man-sections (cmd)
  (nvp-sh:with-man-help cmd 'sync
    (Man-build-section-list)
    Man--sections))

;; when Man finishes, set point in MAN-BUFFER to be
;; after description of CMD
;; (for `nvp-sh-help-more-help' on bash-builtins)
(defun nvp-sh--Man-after-notify (man-buffer cmd &optional display)
  (save-mark-and-excursion
    (with-current-buffer man-buffer
      (goto-char (point-min))
      (catch 'done
        (while (re-search-forward (concat "^[ \t]*" cmd "\\_>") nil 'move)
          (forward-char -1)
          (and (eq 'Man-overstrike (get-text-property (point) 'face))
               (throw 'done nil))))
      (set-window-point (get-buffer-window man-buffer) (point))))
  (prog1 man-buffer
    (and display (display-buffer man-buffer 'not-this-window))))

;; Too much trouble trying to figure out how to make Man
;; run synchronously or redefine Man notify command
(defsubst nvp-sh--Man-notify (man-buffer cmd)
  (run-with-timer 0.2 nil 'nvp-sh--Man-after-notify man-buffer cmd 'display))

;; -------------------------------------------------------------------
;;; Help at point
;;
;; When popup is being displayed, hitting "h" displays buffer with
;; more help for current command.
;; - For bash builtins, this is 'man "bash-builtins"' and the window is
;;   scrolled to the description of the command at point.
;; - For other commands, just call Man on command.

(defvar Man-notify-method)
(defun nvp-sh-help-more-help (cmd)
  (interactive)
  (let ((Man-notify-method 'quiet)
        (Man-prefer-synchronous-call t))
    (nvp-sh:with-bash/man cmd
        (nvp-sh--Man-after-notify (man "bash-builtins") cmd)
      (man cmd))))

;; return docstring for conditional, builtin, or function
;; if PROMPT is non-nil, offer completing read for switches or man section,
;; depending on the CMD
(defun nvp-sh--command-docstring (cmd &optional prompt section recache)
  (let ((message-log-max nil) (inhibit-message t))
    (cond
     ((member cmd '("[[" "["))
      (nvp-sh--conditional-string
       (if prompt (completing-read "Switch: " nvp-sh--conditional-cache)
         (nvp-sh--conditional-switch))))
     (t
      (nvp-sh--function-string
       cmd (if prompt
               (completing-read "Man Section: " (nvp-sh--man-sections cmd) nil t)
             section)
       recache)))))

;;;###autoload
(defun nvp-hap-bash (command &optional arg prefix &rest _args)
  "Lookup help for current command.
With \\[universal-argument], lookup thing point without looking for current
command.."
  (cl-case command
    (thingatpt (if (equal '(4) arg)
                   (thing-at-point 'symbol)
                 (nvp-sh-current-command)))
    (doc-buffer (save-window-excursion
                  (--when-let (nvp-sh-help-more-help arg)
                    (and (buffer-live-p it) (list it :set nil)))))
    ;; - if cmd is '[[' or '[' return help for current switch or all if not found
    ;; - with C-u C-u prompt for 'man' section and recache
    (doc-string (nvp-sh--command-docstring
                 arg (equal '(16) prefix) nil (equal '(16) prefix)))))

;; -------------------------------------------------------------------
;;; Company 

(require 'company)
(require 'company-quickhelp)

;;;###autoload
(defun nvp-sh-company-complete (arg)
  "Temporarily use only sourced / local functions for completion."
  (interactive "P")
  (if arg (call-interactively 'company-sh-comp)
    (company-complete)))

;; Since no doc-buffer is returned by company-capf, rewrite
;; company-quickhelp doc retrieval method to just call man on the
;; current candidates
;; return a company-documentation buffer with either Man output or bash help
;; for a builtin
(defun nvp-sh-doc-buffer (cmd)
  (let ((doc-str (if (nvp-sh--bash-builtin-p cmd)
                     (nvp-sh-bash-builtin-help-sync cmd)
                   (shell-command-to-string (format "man %s" cmd)))))
    (and (not (or (member doc-str '(nil ""))
                  (string-prefix-p "No manual entry" doc-str)))
         (company-doc-buffer doc-str))))

;; FIXME: merge into `nvp-hap'
;; modified `company-quickhelp--doc'
(defun nvp-sh-quickhelp-doc (selected)
  (cl-letf (((symbol-function 'completing-read)
             #'company-quickhelp--completing-read))
    (let ((message-log-max nil) (inhibit-message t))
      (when-let* ((doc-buf (nvp-sh-doc-buffer selected))
                  (doc-and-meta (with-current-buffer doc-buf
                                  (company-quickhelp--docstring-from-buffer
                                   (point-min))))
                  (truncated (plist-get doc-and-meta :truncated))
                  (doc (plist-get doc-and-meta :doc)))
        (unless (member doc '(nil ""))
          (if truncated
              (concat doc "\n\n[...]")
            doc))))))

;; local `nvp-quickhelp-toggle-function' override
(defun nvp-sh-quickhelp-toggle ()
  (cl-letf (((symbol-function 'company-quickhelp--doc) #'nvp-sh-quickhelp-doc))
    ;; flickers the screen - cant use the timer, since it seems
    ;; that lexical binding doesn't work in that case
    ;; (company-quickhelp-manual-begin)
    (company-quickhelp--show)))

;; show help buffer in other window from company-active-map
(defun nvp-sh-company-show-doc-buffer ()
  (interactive)
  (cl-letf (((symbol-function 'company-call-backend)
             #'(lambda (_type selected)
                 (nvp-sh-doc-buffer selected) "*company-documentation*")))
    (company-show-doc-buffer)))

(provide 'nvp-sh-help)
;;; nvp-sh-help.el ends here
