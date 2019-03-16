;;; nvp-debug.el --- gud -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-08 04:38:19>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created:  8 March 2019

;;; Commentary:

;;; FIXME: gud-mode seems to clobber kill-buffer-hooks,
;;         so shell history isn't being saved/read properly

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib)
  (require 'hydra))
(require 'nvp)
(require 'gud)
(nvp-declare "" nvp-indicate-cursor-pre nvp-indicate-cursor-post
  nvp-he-history-setup nvp-comint-setup-history)

;; -------------------------------------------------------------------
;;; GDB REPL

;;;###autoload
(defun nvp-gud-repl-setup (&optional name)
  (interactive
   (list
    (let ((default (or (ignore-errors (symbol-name gud-minor-mode)) "gdb")))
     (read-from-minibuffer
      (nvp-prompt-default "Mode: " default) nil nil nil nil default))))
  (or name (setq name (if gud-minor-mode (symbol-name gud-minor-mode) "gdb")))
  (nvp-he-history-setup :history 'comint-input-ring
                        :bol-fn 'comint-line-beginning-position)
  (nvp-comint-setup-history (concat ".gud_" name "_history") nil 'write))

;;;###autoload(autoload 'nvp-gud-repl-switch "nvp-debug")
(nvp-repl-switch "gud" (:repl-mode 'gud-mode
                        :repl-find-fn
                        #'(lambda ()
                            (and (comint-check-proc gud-comint-buffer)
                                 gud-comint-buffer)))
  ;; FIXME: how to get the buffer returned from interactive call
  ;; and add source buffer property after GDB has started?
  (call-interactively 'gdb))

;;;###autoload(autoload 'nvp-gud-hydra/body "nvp-debug")
;; compiler doesnt understande these functions
(nvp-hydra-set-property 'nvp-gud-hydra)
(with-no-warnings
  (defhydra nvp-gud-hydra (:color amaranth
                           :pre nvp-indicate-cursor-pre
                           :post nvp-indicate-cursor-post)
    ;; vi
    ("h" backward-char nil)
    ("j" next-line nil)
    ("k" previous-line nil)
    ("l" forward-char nil)
    ;; gud
    ("m" gud-many-windows "many-windows mode")
    ("t" gud-tbreak "tbreak")
    ("b" gud-break "break")
    ("d" gud-remove "remove")
    ;; ("D" )
    ("J" gud-jump "jump")
    ("p" gud-print "print")
    ("m" gud-until "move")
    ("n" gud-next "next")
    ("c" gud-cont "cont")
    ("o" gud-finish "out")
    ("r" gud-run "run")
    ("q" nil "quit")))

(provide 'nvp-debug)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-debug.el ends here
