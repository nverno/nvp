;;; nvp-edebug.el --- emacs/elisp debubgging helper -*- lexical-binding: t; -*-

;;; Commentary:

;; emacs/elisp debugging
;; - cc-engine: #<marker at 23505 in cc-engine.el.gz>
;; it would be useful to highlight positions with overlays during debugging
;; see #<marker at 63523 in cc-engine.el.gz>

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (defvar tramp-debug-on-error)
  (defvar tramp-verbose)
  (require 'hydra))
(require 'edebug)
(nvp-decls)
(nvp-declare nvp-help-describe-keymap)

;;; Completion

;; completion table for locals in current frame
(defvar nvp-backtrace-locals-completion-table
  (completion-table-in-turn
   (completion-table-dynamic
    (lambda (_string)
      (-when-let* ((idx (backtrace-get-index))
                   (frame (nth idx backtrace-frames)))
        (backtrace-frame-locals frame)))
    'do-switch-buffer)
   elisp--local-variables-completion-table))

(defun nvp-backtrace-capf ()
  (-when-let ((beg . end) (bounds-of-thing-at-point 'symbol))
    (list beg end
          nvp-backtrace-locals-completion-table
          :exclusive 'no)))

;; (remove-hook 'completion-at-point-functions #'nvp-backtrace-capf)
;; (remove-hook 'eval-expression-minibuffer-setup-hook #'nvp-backtrace-capf)
;; (setq-local completion-at-point-functions '(elisp-completion-at-point t))

;; (add-hook 'completion-at-point-functions #'nvp-backtrace-capf nil t)
;; (remove-hook 'eval-expression-minibuffer-setup-hook
;;           (lambda ()
;;             (setq-local completion-at-point-functions
;;                         '(nvp-backtrace-capf t))))

(define-advice debugger-eval-expression
    (:around (orig-fn &rest args) "local-completion")
  (let ((_capf completion-at-point-functions)
        (completion-at-point-functions '(nvp-backtrace-capf t))))
  (cl-letf (((symbol-value 'elisp--local-variables-completion-table)
             (symbol-value 'nvp-backtrace-locals-completion-table)))
    (funcall-interactively orig-fn args)))


;; (defun nvp-edebug-minibuffer-hook ()

;;   (add-hook 'completion-at-point-functions
;;             #'nvp-edebug-locals-completion-at-point nil t))

;; (add-hook 'eval-expression-minibuffer-setup-hook
;;           #'nvp-edebug-minibuffer-hook)

;; setup eval with elisp minibuffer eval hooks
(defun nvp-edebug-eval-expression (expr)
  (interactive (list (read--expression "Eval: ")))
  (edebug-eval-expression expr))

;;; Debugger

(defvar nvp-backtrace--multi nil)
(defun nvp-backtrace-toggle-multi ()
  (interactive)
  (if (nvp-toggle-variable nvp-backtrace--multi)
      (backtrace-single-line)
    (backtrace-multi-line)))

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

;;; Edebug

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


;;; Launching new emacs instances

(eval-when-compile
  (defmacro nvp-debug:launch-emacs (&rest args)
    (declare (debug defun))
    (let ((args (cons (nvp-with-gnu/w32 "emacs" "runemacs.exe") args)))
      `(call-process-shell-command
        (mapconcat #'shell-quote-argument (delq nil (list ,@args)) " ") nil 0 nil))))

(defun nvp-edebug-launch-bare-emacs (&optional arg)
  "Start new emacs instance loading very minimal init (load paths + some basics).
With prefix open current file."
  (interactive "P")
  (nvp-debug:launch-emacs
   "-Q" "-l" (expand-file-name "init-site.el" nvp/etc) (if arg (buffer-file-name))))

(defun nvp-edebug-launch-new-debug ()
  "Start new emacs in debug mode."
  (interactive)
  (nvp-debug:launch-emacs "--debug-init"))


;;;###autoload(autoload 'nvp-edebug-hydra-emacs/body "nvp-edebug")
(nvp-hydra-set-property 'nvp-edebug-hydra-emacs)
(defhydra nvp-edebug-hydra-emacs (:color blue)
  "
Toggle debugging functions:

^toggle^    ^edebug^              ^other^
^^^^^^^^------------------------------------------------------------
_e_rror      _a_: all defuns      _t_ramp
_q_uit       _l_: all local defs  _d_ebugger
entr_y_      _f_: all forms       _b_are emacs (-l bare-site)
_v_ariable   _c_urrent defun      _n_ew emacs (in debug)
             _u_: unload
"
  ("a" edebug-all-defs)
  ("b" nvp-edebug-launch-bare-emacs)
  ("c" edebug-defun)
  ("d" debug)                           ;debugger
  ("e" toggle-debug-on-error)
  ("f" edebug-all-forms)
  ("l" nvp-edebug-toggle-all-local)
  ("n" nvp-edebug-launch-new-debug)
  ("q" toggle-debug-on-quit)
  ("t" nvp-edebug-tramp-toggle-debug)
  ("u" edebug-uninstall-read-eval-functions)
  ("v" debug-on-variable-change)
  ("y" debug-on-entry))

(provide 'nvp-edebug)
;;; nvp-edebug.el ends here
