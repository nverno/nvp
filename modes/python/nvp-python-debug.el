;;; nvp-python-debug.el --- python debugging -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-03-21 14:17:54>
;; Created: 16 September 2018

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'hydra)
  (defvar nvp-python-debug-breakpoint-string))
(require 'gud)
(require 'nose)
(require 'nvp-python)
(nvp-declare "" nvp-he-history-setup nvp-comint-setup-history)

;;; Debug

;;;###autoload
(defun nvp-python-debug-annotate-pdb ()
  "Highlight break point lines."
  (interactive)
  (highlight-lines-matching-regexp "import i?pu?db")
  (highlight-lines-matching-regexp "i?pu?db.set_trace()"))

;;;###autoload
(defun nvp-python-debug-toggle-breakpoint ()
  "Add or remove a debugging breakpoint at point."
  (interactive)
  (let ((line (thing-at-point 'line)))
    (if (and line (string-match-p nvp-python-debug-breakpoint-string line))
        (kill-whole-line)
      (progn
        (back-to-indentation)
        (insert nvp-python-debug-breakpoint-string)
        (insert "\n")
        (python-indent-line)))))

;; -------------------------------------------------------------------
;;; GDB REPL 

;;;###autoload
(defun nvp-python-pdb-repl-setup ()
  (require 'nvp-comint)
  (nvp-he-history-setup :history 'comint-input-ring
                        :bol-fn 'comint-line-beginning-position)
  (nvp-comint-setup-history ".pdb_history"))

(nvp-repl-switch "gud-pdb" (:repl-mode 'gud-mode
                            :repl-doc "Switch between PDB and source buffer."
                            :repl-find-fn
                            #'(lambda ()
                                (and (comint-check-proc gud-comint-buffer)
                                     gud-comint-buffer)))
  ;; FIXME: buffer returned from interactive call and source
  ;; buffer property after PDB starts?
  (call-interactively 'pdb))
;;;###autoload(autoload 'nvp-gud-pdb-switch "nvp-python-debug")

;; -------------------------------------------------------------------
;;; PDB Hydra

(with-no-warnings
  (defhydra nvp-pdb-hydra (:color amaranth
                           :pre nvp-indicate-cursor-pre
                           :post nvp-indicate-cursor-post)
    ;; movement
    ("h" backward-char nil)
    ("j" next-line nil)
    ("k" previous-line nil)
    ("l" forward-char nil)
    ;; pdb
    ("b" nvp-python-debug-toggle-breakpoint "pdb break")
    ;; gud
    ("C-b" gud-break "break")
    ("c" gud-remove "clear")
    ;; nose
    ("a" nosetests-pdb-all "nose-all")
    ("m" nosetests-pdb-module "nose-mod")
    ("o" nosetests-pdb-one "nose1")
    ;; quit
    ("q" nil "exit")))
(nvp-hydra-set-property 'nvp-pdb-hydra :verbosity 1)

(provide 'nvp-python-debug)
;;; nvp-python-debug.el ends here
