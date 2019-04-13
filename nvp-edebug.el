;;; nvp-edebug.el --- emacs/elisp debubgging helper -*- lexical-binding: t; -*-

;;; Commentary:

;; emacs/elisp debugging
;; - cc-engine: #<marker at 23505 in cc-engine.el.gz>
;; it would be useful to highlight positions with overlays during debugging
;; see #<marker at 63523 in cc-engine.el.gz>

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro)
  (defvar tramp-debug-on-error)
  (defvar tramp-verbose)
  (require 'hydra))
(require 'edebug)
(nvp-declare nvp-help-describe-keymap)

;; setup eval with elisp minibuffer eval hooks
(defun nvp-edebug-eval-expression (expr)
  (interactive (list (read--expression "Eval: ")))
  (edebug-eval-expression expr))

(defun nvp-edebug-help ()
  (interactive)
  (nvp-help-describe-keymap 'edebug-mode-map))

;;;###autoload(autoload 'nvp-edebug-hydra-emacs/body "nvp-edebug")
(nvp-hydra-set-property 'nvp-edebug-hydra-emacs)
(defhydra nvp-edebug-hydra-emacs (:color blue)
  "
Toggle debugging functions:

^emacs^                       ^edebug^               ^tramp/other^
^^^^^^^^------------------------------------------------------------
toggle on _e_rror              _a_: all defuns         _t_ramp
toggle on _q_uit               _l_: all local defs     _i_: ielm
_n_ew emacs (in debug)         _f_: all forms          _SPC_: cancel
_b_are emacs (-l bare-site)    _c_urrent defun         _d_ebugger
toggle on entr_y_
"
  ("a" edebug-all-defs)
  ("b" nvp-edebug-launch-bare-emacs)
  ("c" edebug-defun :color blue)
  ("d" debug :color blue)
  ("e" toggle-debug-on-error)
  ("f" edebug-all-forms)
  ("i" ielm :color blue)
  ("l" nvp-edebug-toggle-all-local)
  ("n" nvp-edebug-launch-new-debug)
  ("q" toggle-debug-on-quit)
  ("SPC" nil)
  ("t" nvp-edebug-tramp-toggle-debug)
  ("y" debug-on-entry :color blue))

(defvar nvp-ede-local-active nil)
(defun nvp-edebug-toggle-all-local ()
  "Toggle debugging of all defuns."
  (interactive)
  (make-local-variable 'edebug-all-defs)
  (edebug-all-defs)
  (setq nvp-ede-local-active (not nvp-ede-local-active))
  (message "Local edebug-all-defs is %s." (if nvp-ede-local-active "on" "off")))

(defun nvp-edebug-tramp-toggle-debug ()
  "Toggle `tramp-debug-on-error' on/off."
  (interactive)
  (ignore-errors
    (when (and (file-remote-p (buffer-file-name)))
      (let ((action (and (not (null tramp-debug-on-error))
                         tramp-debug-on-error)))
        (setq tramp-debug-on-error (not action))
        (setq tramp-verbose (if action 0 10))
        (message "set `tramp-debug-on-error' %s" (if action "off" "on"))))))

;;; Launching new emacs instances
(eval-when-compile
  (defmacro nvp-edebug--launch-emacs (&rest args)
    (declare (debug defun))
    (let ((args (cons (nvp-with-gnu/w32 "emacs" "runemacs.exe") args)))
      `(call-process-shell-command
        (mapconcat #'shell-quote-argument (delq nil (list ,@args)) " ") nil 0 nil))))

(defun nvp-edebug-launch-bare-emacs (&optional arg)
  "Start new emacs instance loading very minimal init (load paths + some basics).
With prefix open current file."
  (interactive "P")
  (nvp-edebug--launch-emacs
   "-Q" "-l" (expand-file-name "init-site.el" nvp/etc) (if arg (buffer-file-name))))

(defun nvp-edebug-launch-new-debug ()
  "Start new emacs in debug mode."
  (interactive)
  (nvp-edebug--launch-emacs "--debug-init"))

(provide 'nvp-edebug)
;;; nvp-edebug.el ends here
