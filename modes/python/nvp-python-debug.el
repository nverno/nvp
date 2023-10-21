;;; nvp-python-debug.el --- python debugging -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-python)
(require 'transient)
;; (require 'gud)

;;;###autoload(autoload 'nvp-python-debug-menu "nvp-python-debug")
(transient-define-prefix nvp-python-debug-menu ()
  "Python debug"
  ;; toggle `gud-pdb-command-name'
  ["Breakpoint"
   ("b" "Toggle" nvp-python-toggle-breakpoint)
   ("h" "Highlight" nvp-python-annotate-breakpoints)
   ("K" "Remove all" nvp-python-remove-breakpoints)]
  ["Profile"
   ("p" "Profile" nvp-python-profile)
   ("c" "Callgraph" nvp-python-callgraph)])

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

;;; PDB Hydra
;; (nvp:hydra-set-property 'nvp-pdb-hydra :verbosity 1)
;; (with-no-warnings
;;   (defhydra nvp-pdb-hydra (:color amaranth
;;                            :pre nvp-indicate-cursor-pre
;;                            :post nvp-indicate-cursor-post)
;;     ;; movement
;;     ("h" backward-char nil)
;;     ("j" next-line nil)
;;     ("k" previous-line nil)
;;     ("l" forward-char nil)
;;     ;; pdb
;;     ("b" nvp-python-toggle-breakpoint "pdb break")
;;     ;; gud
;;     ("C-b" gud-break "break")
;;     ("c" gud-remove "clear")
;;     ;; nose
;;     ("a" nosetests-pdb-all "nose-all")
;;     ("m" nosetests-pdb-module "nose-mod")
;;     ("o" nosetests-pdb-one "nose1")
;;     ;; quit
;;     ("q" nil "exit")))

;;; Debug

;;;###autoload
(defun nvp-python-annotate-breakpoints (&optional arg)
  "Highlight break point lines."
  (interactive "P")
  (if arg
      (hi-lock-unface-buffer t)
    (highlight-lines-matching-regexp "import i?pu?db")
    (highlight-lines-matching-regexp "i?pu?db.set_trace()")))

(defun nvp-python-remove-breakpoints ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (concat "^\\s-*" nvp-python-breakpoint-string "[ \t]*$") nil t)
      (delete-line))))

;;;###autoload
(defun nvp-python-toggle-breakpoint ()
  "Add or remove a debugging breakpoint at point."
  (interactive)
  (let ((line (thing-at-point 'line)))
    (if (and line (string-match-p nvp-python-breakpoint-string line))
        (delete-line)
      (progn
        (back-to-indentation)
        (insert nvp-python-breakpoint-string)
        (insert "\n")
        (python-indent-line)))))

(provide 'nvp-python-debug)
;;; nvp-python-debug.el ends here
