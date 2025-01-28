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
(require 'edebug)
(require 'transient)
(nvp:decls :p (nvp-treesit smie treesit lsp tramp)
           :f (treesit-buffer-root-node smie-config-show-indent)
           :v (tramp-debug-on-error tramp-verbose))

(autoload 'nvp-describe-keymap "nvp-help")
(autoload '-map "dash")

;;; Edebug
;; Completion for frame-locals in edebug buffer
(defun nvp@edebug-locals (orig-fn expr)
  (interactive
   (let ((hookfun
          (lambda ()
            (add-function
             :around (local 'elisp--local-variables-completion-table)
             ;; Needs to switch buffer before completing in minibuffer
             #'nvp@do-switch-buffer))))
     (unwind-protect
         (progn (add-hook 'eval-expression-minibuffer-setup-hook hookfun)
                (list (read--expression "Eval (in stack): ")))
       (remove-hook 'eval-expression-minibuffer-setup-hook hookfun))))
  (funcall orig-fn expr current-prefix-arg))

(advice-add 'edebug-eval-expression :around #'nvp@edebug-locals)

(defun nvp-edebug-help ()
  (interactive)
  (nvp-describe-keymap 'edebug-mode-map))

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


;;; Menu Toggles: projectile, filenotify, hap, debug, edebug settings
(defvar projectile-verbose)
(defvar file-notify-debug)
(defvar nvp-hap-verbose)
(defvar server-log)

(nvp:transient-define-vars nvp--menu
  (byte-compile-generate-call-tree :choices '(nil t prompt))
  (byte-compile-call-tree-sort
   :reader (lambda (prompt &rest args)
             (intern-soft
              (apply #'completing-read
                     prompt '(name callers calls calls+callers) nil t args))))
  debug-on-message
  (edebug-on-error . t)
  (edebug-on-quit . t)
  (edebug-unwrap-results . t)
  (edebug-trace . t)
  (edebug-sit-on-break . t)
  edebug-global-break-condition
  (file-notify-debug . t)
  (nvp-hap-verbose . t)
  (projectile-verbose . t)
  (server-log . t)
  (url-debug
   :reader (lambda (prompt &rest args)
             (if (null current-prefix-arg) (not url-debug)
               (->> (apply #'completing-read-multiple
                           prompt '(http dav retrieval handlers) nil t args)
                    (mapcar #'intern-soft)
                    (seq-uniq))))))


;;; Tramp
(defun nvp--menu-tramp-debug-on-error ()
  "Toggle `tramp-debug-on-error' on/off."
  (interactive)
  (ignore-errors
    (when (and (file-remote-p (buffer-file-name)))
      (let ((action (and (not (null tramp-debug-on-error))
                         tramp-debug-on-error)))
        (setq tramp-debug-on-error (not action))
        (setq tramp-verbose (if action 0 10))
        (message "Tramp debug on error %s" (if action "disabled" "enabled"))))))


;;; Emacs
(transient-define-suffix nvp-edebug-menu-launch (args)
  "Launch emacs with ARGS."
  (interactive (list (transient-args 'nvp-edebug-menu)))
  (let* ((grps (--group-by (string-prefix-p "--nvp" it) args))
         (args (concat
                (mapconcat #'shell-quote-argument (cdr (assq nil grps)) " ")
                " --eval \"(setq debug-on-error t)\""))
         (dummy (cdr (assq t grps))))
    (when (member "--nvp-current" dummy)
      (setq args (concat args " " (buffer-file-name))))
    (call-process-shell-command (concat "emacs " args) nil 0 nil)))

(transient-define-infix nvp-edebug-menu-load ()
  "Load init file."
  :description "Load init"
  :class 'transient-option
  :argument "--load="
  :reader (lambda (prompt initial-input history)
            (-> (->> (completing-read prompt '("bare" "site" "funcs")
                       nil t initial-input history)
                     (format "etc/init-%s.el"))
                (expand-file-name user-emacs-directory))))


;;; Smie
(defun nvp-edebug-smie--toggle-verbose ()
  "Toggle smie indent verbose."
  (interactive)
  (if (advice-member-p #'smie-config-show-indent 'smie-indent-line)
      (progn (advice-remove 'smie-indent-line #'smie-config-show-indent)
             (message "Disabled verbose indent"))
    (advice-add 'smie-indent-line :after #'smie-config-show-indent)
    (message "Enabled verbose indent")))

;; Useful smie debugging output: #<marker at 7444 in tuareg-opam.el>
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
(defun nvp--native-comp-read-level (prompt &rest _)
  (string-to-number
   (completing-read
    prompt (--map (number-to-string it) (number-sequence 0 3)) nil t)))

(defvar native-comp-async-report-warnings-errors)
(defvar native-comp-always-compile)
(nvp:transient-define-vars nvp--menu
  (native-comp-verbose :reader #'nvp--native-comp-read-level)
  (native-comp-debug :reader #'nvp--native-comp-read-level)
  native-comp-async-report-warnings-errors
  native-comp-always-compile)

(transient-define-prefix nvp-native-comp-menu ()
  [[ :if-mode emacs-lisp-mode "Compile"
     ("c" "Compile" emacs-lisp-native-compile)
     ("l" "Compile and load" emacs-lisp-native-compile-and-load)]
   ["Settings"
    (":v" "Verbosity" nvp--menu-native-comp-verbose)
    (":d" "Debug" nvp--menu-native-comp-debug)
    (":w" "Report async warnings/errors"
     nvp--menu-native-comp-async-report-warnings-errors)
    (":a" "Always compile" nvp--menu-native-comp-always-compile)]])


;;;###autoload(autoload 'nvp-edebug-menu "nvp-edebug" nil t)
(transient-define-prefix nvp-edebug-menu ()
  "Toggle or run elisp debugging."
  :value '("--quick" "--debug-init" "--nvp-current")
  :refresh-suffixes t
  [["Debug"
    ("e" "On error" toggle-debug-on-error)
    ("q" "On quit" toggle-debug-on-quit)
    ("y" "On entry" debug-on-entry)
    ("v" "On variable" debug-on-variable-change)
    ("M" "On message" nvp--menu-debug-on-message)
    ("Y" "Off entry" cancel-debug-on-entry)
    ("V" "Off variable" cancel-debug-on-variable-change)
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
    (":e" "EDebug on error" nvp--menu-edebug-on-error)
    (":q" "EDebug on quit" nvp--menu-edebug-on-quit)
    (":u" "Unwrap results" nvp--menu-edebug-unwrap-results)
    (":t" "Trace" nvp--menu-edebug-trace)
    (":p" "Pause on break" nvp--menu-edebug-sit-on-break)
    (":b" "Break condition" nvp--menu-edebug-global-break-condition)]
   ["Trace"
    ("t" "Trace" nvp-trace-menu :transient transient--do-replace)
    ""
    "Call Tree"
    ("C" "Display call tree" display-call-tree
     :if-non-nil byte-compile-generate-call-tree)
    (":s" "Sort call tree" nvp--menu-byte-compile-call-tree-sort
     :if-non-nil byte-compile-generate-call-tree)
    (":c" "Generate call tree" nvp--menu-byte-compile-generate-call-tree)]]
  [["Menus"
    ("/t" "Tree-sitter" nvp-treesit-menu :transient transient--do-replace
     :if (lambda () (ignore-errors (treesit-buffer-root-node))))
    ("/r" "Transient" nvp-transient-menu :transient transient--do-replace)
    ("/l" "Lsp" nvp-lsp-menu :if (lambda () (featurep 'lsp-mode))
     :transient transient--do-replace)
    ("/c" "Native comp" nvp-native-comp-menu :transient transient--do-replace)
    ("/s" "Smie" nvp-edebug-smie :transient transient--do-replace
     :if (lambda () (eq 'smie-indent-line indent-line-function)))]
   ["Toggle Debug"
    (",s" "Server" nvp--menu-server-log)
    (",h" "Hap" nvp--menu-hap-verbose
     :if (lambda () (boundp 'nvp-hap-verbose)))
    (",p" "Projectile" nvp--menu-projectile-verbose
     :if-non-nil projectile-mode)
    (",f" "Filenotify" nvp--menu-file-notify-debug)
    (",u" "Url debug" nvp--menu-url-debug :if (lambda () (boundp 'url-debug)))
    (",t" "Tramp" nvp--menu-tramp-debug-on-error
     :if (lambda () (--when-let (buffer-file-name) (file-remote-p it))))]
   ["Recent Cmds"
    ("xc" "Used minibuf" list-command-history)
    ("xl" "Lossage" view-lossage)]
   ["Emacs"
    ("L" "Launch" nvp-edebug-menu-launch)
    ("-d" "Debug init" "--debug-init")
    ("-Q" "No init" "--quick")
    ("-c" "Open current" "--nvp-current")
    ("-l" nvp-edebug-menu-load)]])

(provide 'nvp-edebug)
;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not docstrings redefine)
;; End:
;;; nvp-edebug.el ends here
