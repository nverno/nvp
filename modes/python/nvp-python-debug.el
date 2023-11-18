;;; nvp-python-debug.el --- python debugging -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'transient)
(nvp:decls :p (gud) :v (nvp-python-breakpoint-string))

;;;###autoload(autoload 'nvp-python-debug-menu "nvp-python-debug")
(transient-define-prefix nvp-python-debug-menu ()
  "Python debug"
  ;; toggle `gud-pdb-command-name'
  ["Debugger"
   ("d" "Pdb" pdb)]
  ["Breakpoint"
   ("b" "Toggle" nvp-python-toggle-breakpoint)
   ("h" "Highlight" nvp-python-annotate-breakpoints)
   ("K" "Remove all" nvp-python-remove-breakpoints)]
  ["Profile"
   ("p" "Profile" nvp-python-profile)
   ("c" "Callgraph" nvp-python-callgraph)])

;;; Compile

;; if debug breakpoint is detected in source, then compile in comint mode
;; (define-advice compile (:around (orig cmd &optional comint) "py-maybe-debug")
;;   (when (derived-mode-p major-mode 'python-mode)
;;     (save-excursion
;;       (save-match-data
;;         (goto-char (point-min))
;;         (if (re-search-forward
;;              (concat "^\\s-*" nvp-python-breakpoint-string "[ \t]*$") nil t)
;;             (funcall orig cmd t)
;;           (funcall orig cmd comint))))))

;;; GDB REPL 

;; FIXME: use something like `nvp-gdb-init'. Need to add option to choose
;; from multiple repls associated with modes
(nvp:repl-switch "gud-pdb"
  ( :repl-mode 'gud-mode
    :repl-doc "Switch between PDB and source buffer."
    :repl-find-fn
    #'(lambda ()
        (and (comint-check-proc gud-comint-buffer)
             gud-comint-buffer)))
  ;; FIXME: buffer returned from interactive call and source
  ;; buffer property after PDB starts?
  (call-interactively 'pdb))

;;; Breakpoints

;;;###autoload
(defun nvp-python-annotate-breakpoints (&optional arg)
  "Highlight break point lines."
  (interactive "P")
  (if arg
      (hi-lock-unface-buffer t)
    (highlight-lines-matching-regexp "import i?pu?db")
    (highlight-lines-matching-regexp "i?pu?db.set_trace()")))

(provide 'nvp-python-debug)
;;; nvp-python-debug.el ends here
