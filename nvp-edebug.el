;;; nvp-edebug.el --- emacs/elisp debubgging helper -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Emacs/elisp debugging
;;
;;; Notes
;; - cc-engine: #<marker at 23505 in cc-engine.el.gz>
;; it would be useful to highlight positions with overlays during debugging
;; see #<marker at 63523 in cc-engine.el.gz>
;;
;; edebug-x.el: tabulated list of breakpoints looks useful
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'elisp-mode)
(require 'backtrace)
(require 'edebug)
(require 'transient)
(nvp:decls :v (tramp-debug-on-error tramp-verbose)
           :f (nvp-help-describe-keymap))

;; -------------------------------------------------------------------
;;; Debugger

;; Completion for frame locals in *Backtrace*
;; gather locals relevant to frame at point:
;; this is locals in the frames at higher indices (not including index at point)
(defun nvp-backtrace--local-variables (_string)
  (when backtrace-frames
    (--mapcat
     (-some->> (backtrace-frame-locals it) (--map (car it)))
     (nthcdr (1+ (backtrace-get-index)) backtrace-frames))))

(defvar nvp-backtrace-locals-completion-table
  (completion-table-dynamic #'nvp-backtrace--local-variables 'do-switch-buffer))

;; override interactive spec of `debugger-eval-expression' to use
;; `nvp@backtrace-eval' instead
(defun nvp@backtrace-eval (orig-fn exp &optional nframes)
  (interactive
   (let ((elisp--local-variables-completion-table
          nvp-backtrace-locals-completion-table))
     (list (read--expression "[nvp] Eval in stack frame: "))))
  (apply orig-fn (list exp nframes)))

(advice-add 'debugger-eval-expression :around #'nvp@backtrace-eval)

;; starts off as single-line
(defvar-local nvp-backtrace--multi t)
(defun nvp-backtrace-toggle-multi ()
  (interactive)
  (if (nvp:toggle-variable nvp-backtrace--multi)
      (backtrace-single-line)
    (backtrace-multi-line)))


;; -------------------------------------------------------------------
;;; Edebug

;; Completion for frame-locals in edebug buffer
;; needs to switch buffer before completing in minibuffer
(advice-add 'edebug-eval-expression :around
            (nvp:def nvp@edebug-locals (orig-fn expr)
              (interactive
               (let ((hookfun
                      (lambda ()
                        (add-function
                         :around
                         (local 'elisp--local-variables-completion-table)
                         #'nvp@do-switch-buffer))))
                 (unwind-protect
                     (progn
                       (add-hook 'eval-expression-minibuffer-setup-hook hookfun)
                       (list (read--expression "[nvp] Eval in stack: ")))
                   (remove-hook 'eval-expression-minibuffer-setup-hook hookfun))))
              (funcall orig-fn expr)))

(defun nvp-edebug-help ()
  (interactive)
  (nvp-help-describe-keymap 'edebug-mode-map))

(defvar-local nvp-edebug--all-defs nil)
(defun nvp-edebug-toggle-all-local ()
  "Toggle debugging of all defuns."
  (interactive)
  (make-local-variable 'edebug-all-defs)
  (if (setq nvp-edebug--all-defs (not nvp-edebug--all-defs))
      (edebug-install-read-eval-functions)
    (edebug-uninstall-read-eval-functions))
  (edebug-all-defs)
  (message "Edebug all defs %s locally"
           (if nvp-edebug--all-defs "enabled" "disabled")))

;; -------------------------------------------------------------------
;;; Emacs / Tramp

(defun nvp-edebug-tramp-toggle-debug ()
  "Toggle `tramp-debug-on-error' on/off."
  (interactive)
  (ignore-errors
    (when (and (file-remote-p (buffer-file-name)))
      (let ((action (and (not (null tramp-debug-on-error))
                         tramp-debug-on-error)))
        (setq tramp-debug-on-error (not action))
        (setq tramp-verbose (if action 0 10))
        (message "Tramp debug on error %s" (if action "disabled" "enabled"))))))

(transient-define-suffix nvp-edebug-emacs--launch (args)
  "Launch emacs with ARGS."
  (interactive (list (transient-args 'nvp-edebug-emacs)))
  (let* ((grps (--group-by (string-prefix-p "--dummy" it) args))
         (args (mapconcat #'shell-quote-argument (cdr (assq nil grps)) " "))
         (dummy (cdr (assq t grps))))
    (when (member "--dummy-current" dummy)
      (setq args (concat args " " (buffer-file-name))))
    (call-process-shell-command (concat "emacs " args) nil 0 nil)))

(transient-define-infix nvp-edebug-emacs--load ()
  "Load init file."
  :description "Load init"
  :class 'transient-option
  :argument "--load="
  :reader
  (lambda (prompt initial-input history)
    (expand-file-name 
     (format
      "etc/init-%s.el"
      (completing-read prompt '("bare" "site" "funcs") nil t initial-input history))
     user-emacs-directory)))

;;;###autoload(autoload 'nvp-edebug-emacs "nvp-edebug")
(transient-define-prefix nvp-edebug-emacs ()
  "Toggle or run elisp debugging."
  :value '("--quick" "--debug-init" "--dummy-current")
  [["Toggle"
    ("e" "Debug on error" toggle-debug-on-error)
    ("q" "Debug on quit" toggle-debug-on-quit)
    ("y" "Debug on entry" debug-on-entry)
    ("v" "Watch variable" debug-on-variable-change)
    ("V" "Cancel watch" cancel-debug-on-variable-change)]
   ["Edebug"
    ("a" "All defs" edebug-all-defs)
    ("l" "All local defs" nvp-edebug-toggle-all-local)
    ("f" "All forms" edebug-all-forms)
    ("c" "Current defun" edebug-defun)
    ("I" "Edebug install" edebug-install-read-eval-functions)
    ("U" "Edebug uninstall" edebug-uninstall-read-eval-functions)]
   ["Other"
    ("t" "Toggle tramp debug" nvp-edebug-tramp-toggle-debug)
    ("d" "Debugger" debug)]
   ["Launch"
    ("-d" "Enable debugger during init" "--debug-init")
    ("-Q" "No init" "--quick")
    ("-c" "Open current file" "--dummy-current")
    ("-l" nvp-edebug-emacs--load)
    ("L" "Launch" nvp-edebug-emacs--launch)]])

(provide 'nvp-edebug)
;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not docstrings redefine)
;; End:
;;; nvp-edebug.el ends here
