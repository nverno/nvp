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
(require 'nvp)
(require 'elisp-mode)
(require 'backtrace)
(require 'edebug)
(require 'transient)
(nvp:decls :p (nvp-treesit smie treesit lsp tramp)
           :f (treesit-buffer-root-node smie-config-show-indent)
           :v (tramp-debug-on-error tramp-verbose))
(nvp:auto "nvp-trace" nvp-trace-menu)
(nvp:auto "nvp-help" nvp-help-describe-keymap)
(nvp:auto "dash" -map)

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
(defun nvp@edebug-locals (orig-fn expr)
  (interactive
   (let ((hookfun
          (lambda ()
            (add-function
             :around (local 'elisp--local-variables-completion-table)
             #'nvp@do-switch-buffer))))
     (unwind-protect
         (progn (add-hook 'eval-expression-minibuffer-setup-hook hookfun)
                (list (read--expression "[nvp] Eval in stack: ")))
       (remove-hook 'eval-expression-minibuffer-setup-hook hookfun))))
  (funcall orig-fn expr))

(advice-add 'edebug-eval-expression :around #'nvp@edebug-locals)

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

;;; Toggle: projectile, filenotify, edebug settings

(defvar projectile-verbose)
(defvar file-notify-debug)
(defvar nvp-hap-verbose)

(nvp:transient-toggle nvp-edebug-menu
  edebug-on-error edebug-on-quit edebug-unwrap-results edebug-trace
  edebug-sit-on-break
  byte-compile-generate-call-tree
  projectile-verbose
  file-notify-debug
  nvp-hap-verbose)

(transient-define-infix nvp-edebug-menu--global-break-condition ()
  :class 'transient-lisp-variable
  :variable 'edebug-global-break-condition)

;;; Tramp
(defun nvp-edebug-menu--toggle-tramp ()
  "Toggle `tramp-debug-on-error' on/off."
  (interactive)
  (ignore-errors
    (when (and (file-remote-p (buffer-file-name)))
      (let ((action (and (not (null tramp-debug-on-error))
                         tramp-debug-on-error)))
        (setq tramp-debug-on-error (not action))
        (setq tramp-verbose (if action 0 10))
        (message "Tramp debug on error %s" (if action "disabled" "enabled"))))))

;;; Url
(transient-define-infix nvp-edebug-menu--toggle-url-debug ()
  "Toggle `url-debug' on/off."
  :description "Toggle URL debug"
  :class 'transient-lisp-variable
  :variable 'url-debug
  :reader
  (lambda (prompt)
    (if (null current-prefix-arg) (not url-debug)
      (seq-uniq
       (mapcar #'intern-soft
               (completing-read-multiple prompt '(http dav retrieval handlers)))))))

;;; Emacs
(transient-define-suffix nvp-edebug-menu--launch (args)
  "Launch emacs with ARGS."
  (interactive (list (transient-args 'nvp-edebug-menu)))
  (let* ((grps (--group-by (string-prefix-p "--dummy" it) args))
         (args (concat
                (mapconcat #'shell-quote-argument (cdr (assq nil grps)) " ")
                " --eval \"(setq debug-on-error t)\""))
         (dummy (cdr (assq t grps))))
    (when (member "--dummy-current" dummy)
      (setq args (concat args " " (buffer-file-name))))
    (call-process-shell-command (concat "emacs " args) nil 0 nil)))

(transient-define-infix nvp-edebug-menu--load ()
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

(defun nvp-edebug-smie--toggle-verbose ()
  "Toggle smie indent verbose."
  (interactive)
  (if (advice-member-p #'smie-config-show-indent 'smie-indent-line)
      (progn (advice-remove 'smie-indent-line #'smie-config-show-indent)
             (message "Disabled verbose indent"))
    (advice-add 'smie-indent-line :after #'smie-config-show-indent)
    (message "Enabled verbose indent")))

(transient-define-prefix nvp-edebug-smie ()
  "Smie debug"
  [["Smie"
    ("v" "Toggle indent verbose" nvp-edebug-smie--toggle-verbose :transient t)
    ("d" "Debug smie-rules-function" smie-edebug)
    ("?" "Show rules at point" smie-config-show-indent :transient t)]
   ["Guess"
    ("g" "Guess config" smie-config-guess :transient t)
    ("S" "Set indent" smie-config-set-indent :transient t)
    ("s" "Save config" smie-config-save)]])

;;; Native compile
(defun nvp-native-comp--read-level (prompt &rest _)
  (string-to-number
   (completing-read
    prompt (--map (number-to-string it) (number-sequence 0 3)) nil t)))

(transient-define-infix nvp-native-comp-menu--verbosity ()
  :class 'transient-lisp-variable
  :variable 'native-comp-verbose
  :reader #'nvp-native-comp--read-level)

(transient-define-infix nvp-native-comp-menu--debug ()
  :class 'transient-lisp-variable
  :variable 'native-comp-debug
  :reader #'nvp-native-comp--read-level)

(defvar native-comp-async-report-warnings-errors)
(defvar native-comp-always-compile)
(nvp:transient-toggle nvp-native-comp-menu
  native-comp-async-report-warnings-errors
  native-comp-always-compile)

(transient-define-prefix nvp-native-comp-menu ()
  [[ :if-mode emacs-lisp-mode "Compile"
     ("c" "Compile" emacs-lisp-native-compile)
     ("l" "Compile and load" emacs-lisp-native-compile-and-load)]
   ["Settings"
    (":v" "Verbosity" nvp-native-comp-menu--verbosity)
    (":d" "Debug" nvp-native-comp-menu--debug)
    (":w" "Report async warnings/errors"
     nvp-native-comp-menu--toggle-native-comp-async-report-warnings-errors)
    (":a" "Always compile"
     nvp-native-comp-menu--toggle-native-comp-always-compile)]])

;;;###autoload(autoload 'nvp-edebug-menu "nvp-edebug")
(transient-define-prefix nvp-edebug-menu ()
  "Toggle or run elisp debugging."
  :value '("--quick" "--debug-init" "--dummy-current")
  [["Debug"
    ("e" "On error" toggle-debug-on-error)
    ("q" "On quit" toggle-debug-on-quit)
    ("y" "On entry" debug-on-entry)
    ("v" "Var. watch" debug-on-variable-change)
    ("V" "Cancel var." cancel-debug-on-variable-change)
    ("D" "Debugger" debug)]
   ["Edebug"
    ("m" "Active menu" nvp-edebug-active-menu :transient transient--do-replace)
    ("d" "Defun" edebug-defun)
    ("l" "Local defs" nvp-edebug-toggle-all-local)
    ("a" "All defs" edebug-all-defs)
    ("f" "All forms" edebug-all-forms)
    ("I" "Install" edebug-install-read-eval-functions)
    ("U" "Uninstall" edebug-uninstall-read-eval-functions)]
   ["Edebug config"
    (":e" "Debug on error" nvp-edebug-menu--toggle-edebug-on-error)
    (":q" "Debug on quit" nvp-edebug-menu--toggle-edebug-on-quit)
    (":u" "Unwrap results" nvp-edebug-menu--toggle-edebug-unwrap-results)
    (":t" "Trace" nvp-edebug-menu--toggle-edebug-trace)
    (":p" "Pause on break" nvp-edebug-menu--toggle-edebug-sit-on-break)
    (":b" "Break condition" nvp-edebug-menu--global-break-condition)]
   ["Trace"
    ("t" "Trace" nvp-trace-menu :transient transient--do-replace)
    (":c" "Generate callgraph"
     nvp-edebug-menu--toggle-byte-compile-generate-call-tree)]]
  [["Other"
    ("/tran" "Transient" nvp-transient-menu :transient transient--do-replace)
    ("/lsp" "Lsp" nvp-lsp-menu :if (lambda () (featurep 'lsp-mode))
     :transient transient--do-replace)
    ("/tree" "Tree-sitter" nvp-treesit-menu :transient transient--do-replace
     :if (lambda () (ignore-errors (treesit-buffer-root-node))))
    ("/hap" "Hap" nvp-edebug-menu--toggle-nvp-hap-verbose
     :if (lambda () (boundp 'nvp-hap-verbose)))
    ("/file" "Filenotify debug" nvp-edebug-menu--toggle-file-notify-debug)
    ("/proj" "Projectile verbose" nvp-edebug-menu--toggle-projectile-verbose
     :if-non-nil projectile-mode)
    ("/comp" "Native comp" nvp-native-comp-menu :transient transient--do-replace)
    ("/smie" "Smie" nvp-edebug-smie :transient transient--do-replace
     :if (lambda () (eq 'smie-indent-line indent-line-function)))
    ("/url" nvp-edebug-menu--toggle-url-debug :if (lambda () (boundp 'url-debug)))
    ("/tramp" "Toggle tramp debug" nvp-edebug-menu--toggle-tramp
     :if (lambda () (--when-let (buffer-file-name) (file-remote-p it))))]
   ["Emacs"
    ("-d" "Enable debugger during init" "--debug-init")
    ("-Q" "No init" "--quick")
    ("-c" "Open current file" "--dummy-current")
    ("-l" nvp-edebug-menu--load)
    ("L" "Launch" nvp-edebug-menu--launch)]])

(provide 'nvp-edebug)
;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not docstrings redefine)
;; End:
;;; nvp-edebug.el ends here
