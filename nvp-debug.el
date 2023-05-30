;;; nvp-debug.el --- gud -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; FIXME: gud-mode seems to clobber kill-buffer-hooks,
;;         so shell history isn't being saved/read properly
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'gud)
(nvp:decls)

;;; GDB REPL

;;;###autoload
(defun nvp-gud-repl-setup (&optional name)
  (interactive
   (list
    (let ((default (or (ignore-errors (symbol-name gud-minor-mode)) "gdb")))
     (read-from-minibuffer
      (nvp:prompt-default "Mode: " default) nil nil nil nil default))))
  (or name (setq name (if gud-minor-mode (symbol-name gud-minor-mode) "gdb")))
  (nvp-comint-setup-history (concat ".gud_" name "_history")))


;;;###autoload(autoload 'nvp-gud-hydra/body "nvp-debug")
;; compiler doesnt understande these functions
(nvp:hydra-set-property 'nvp-gud-hydra)
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
