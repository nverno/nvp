;;; nvp-debug.el --- gud -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; FIXME: gud-mode seems to clobber kill-buffer-hooks,
;;         so shell history isn't being saved/read properly
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'transient)
(require 'nvp)
(nvp:decls :p (gud gdb))

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

;; -------------------------------------------------------------------
;;; Transient

(eval-when-compile
  (defmacro nvp:def-gud-menu ()
    (declare (indent defun))
    (cl-macrolet ((cmd (key desc command)
                    `(list ,key ,desc ',command
                           :if (lambda () (fboundp ',command)))))
      (let ((menu
             (list
              (vector
               :if-non-nil 'gud-minor-mode
               (vector "Go"
                       (cmd "n" "Next line" gud-next)
                       (cmd "N" "Next instruction" gud-nexti)
                       (cmd "s" "Step line" gud-step)
                       (cmd "S" "Step instruction" gud-stepi)
                       (cmd "r" "Run" gud-run)
                       (cmd "g" "Continue-run" gud-go)
                       (cmd "c" "Continue" gud-continue)
                       (cmd "j" "Jump" gud-jump)
                       (cmd "u" "Until" gud-until)
                       (cmd "f" "Finish function" gud-finish))
               (vector "Break"
                       (cmd "b" "Break" gud-break)
                       (cmd "t" "Temp break" gud-tbreak)
                       (cmd "w" "Watch" gud-watch)
                       (cmd "D" "Remove" gud-remove))
               (vector "Print"
                       (cmd "p" "Print" gud-print)
                       (cmd "p" "Print" gud-pp) ; elisp
                       (cmd "*" "Dump dereference" gud-pstar)
                       (cmd "x" "Execute" gud-statement)))
              (vector
               :if-non-nil 'gud-minor-mode
               (vector "Stack"
                       (cmd "u" "Up frame" gud-up)
                       (cmd "d" "Down frame" gud-down))
               (vector "GUD"
                       (cmd "R" "Refresh" gud-refresh)
                       (cmd "k" "Stop" gud-stop-subjob)
                       (cmd "?" "Info (debug)" gud-goto-info))
               (vector
                :if (lambda ()
                      (memq gud-minor-mode
                            '(gdbmi lldb guiler dbx sdb xdb pdb)))
                "Layout"
                '("T" "Show GUD tooltips" gud-tooltip-mode))))))
        `(transient-define-prefix nvp-gud-menu ()
           "GUD"
           :info-manual "Info (debugger)"
           ,@menu)))))

;;;###autoload(autoload 'nvp-gud-menu "nvp-debug")
(nvp:def-gud-menu)

(provide 'nvp-debug)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-debug.el ends here
