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
(nvp-decls :v (tramp-debug-on-error tramp-verbose)
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
;; `nvp/backtrace-eval' instead
(defun nvp/backtrace-eval (orig-fn exp &optional nframes)
  (interactive
   (let ((elisp--local-variables-completion-table
          nvp-backtrace-locals-completion-table))
     (list (read--expression "[nvp] Eval in stack frame: "))))
  (apply orig-fn (list exp nframes)))

(advice-add 'debugger-eval-expression :around #'nvp/backtrace-eval)

;; starts off as single-line
(defvar-local nvp-backtrace--multi t)
(defun nvp-backtrace-toggle-multi ()
  (interactive)
  (if (nvp-toggle-variable nvp-backtrace--multi)
      (backtrace-single-line)
    (backtrace-multi-line)))


;; -------------------------------------------------------------------
;;; Edebug

;; Completion for frame-locals in edebug buffer
;; needs to switch buffer before completing in minibuffer
(advice-add 'edebug-eval-expression :around
            (nvp-def nvp/edebug-locals (orig-fn expr)
              (interactive
               (let ((hookfun
                      (lambda ()
                        (add-function
                         :around
                         (local 'elisp--local-variables-completion-table)
                         #'nvp/do-switch-buffer))))
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
