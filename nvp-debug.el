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
       (nvp:prompt-default "Mode: " default) default nil nil nil default))))
  (or name (setq name (if gud-minor-mode (symbol-name gud-minor-mode) "gdb")))
  (nvp-comint-setup-history (concat ".gud_" name "_history")))

;; -------------------------------------------------------------------
;;; Breakpoint statements

(defvar-local nvp-debug-breakpoint-string nil)

;;;###autoload
(defun nvp-debug-toggle-breakpoint (&optional conditional)
  (interactive "P")
  (unless nvp-debug-breakpoint-string (user-error "unimplemented"))
  (let ((line (thing-at-point 'line)))
    (if (and line (string-match-p
                   (car (split-string nvp-debug-breakpoint-string "%s")) line))
        (delete-line)
      (let ((dbg-str (format nvp-debug-breakpoint-string
                         (if conditional "${1:cond}" ""))))
       (progn
         (goto-char (line-beginning-position))
         (open-line 1)
         (indent-according-to-mode)
         (if conditional (yas-expand-snippet dbg-str)
           (insert dbg-str)))))))

(defun nvp-debug-remove-breakpoints (&optional breakpoint-prefix)
  (interactive (list (if current-prefix-arg (read-string "Breakpoint prefix: ")
                       (and nvp-debug-breakpoint-string
                            (car (split-string nvp-debug-breakpoint-string "%s"))))))
  (unless breakpoint-prefix (user-error "unimplemented"))
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (concat "^\\s-*" breakpoint-prefix) nil t)
        (cl-incf count)
        (delete-line)))
    (message "Remove %d breakpoints" count)))

;; -------------------------------------------------------------------
;;; Transient

(nvp:transient-toggle nvp-gud-menu
  gud-highlight-current-line)

(defun nvp-gud-refresh (&optional arg)
  (interactive "P")
  (let ((display-buffer-overriding-action
         '(nil . ((inhibit-same-window . t)))))
    (funcall-interactively #'gud-refresh arg)))

(eval-when-compile
  (defmacro nvp:def-gud-menu ()
    (declare (indent defun))
    (cl-macrolet ((cmd (key desc command &optional exit)
                    `(list ,key ,desc ',command
                           :if (lambda () (fboundp ',command)) :transient ,(not exit))))
      (let ((menu
             (list
              (vector
               :if (lambda () (bound-and-true-p gud-minor-mode))
               (vector "Go"
                       (cmd "n" "Next line (skip func)" gud-next)
                       (cmd "N" "Next instruction" gud-nexti)
                       (cmd "s" "Step line (into func)" gud-step)
                       (cmd "S" "Step instruction" gud-stepi)
                       (cmd "r" "Run" gud-run)
                       (cmd "g" "Continue-run" gud-go)
                       (cmd "c" "Continue" gud-cont)
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
                       (cmd "P" "Print" gud-pp) ; elisp
                       (cmd "t" "Trace" gud-trace)
                       (cmd "*" "Dump dereference" gud-pstar)
                       (cmd "x" "Execute" gud-statement)))
              (vector
               :if (lambda () (bound-and-true-p gud-minor-mode))
               (vector "Stack"
                       (cmd "u" "Up frame" gud-up)
                       (cmd "d" "Down frame" gud-down))
               (vector "GUD"
                       (cmd "l" "Refresh" nvp-gud-refresh)
                       (cmd "k" "Stop" gud-stop-subjob 'exit)
                       (cmd "?" "Info (debug)" gud-goto-info 'exit)))
              ["Settings"
               (":l" "Highlight current line"
                nvp-gud-menu--toggle-gud-highlight-current-line
                :if (lambda () (bound-and-true-p gud-minor-mode)))
               ;; :if (lambda () (memq gud-minor-mode '(gdbmi lldb guiler dbx sdb xdb pdb)))
               (":t" "Enable GUD tooltip mode" gud-tooltip-mode)])))
        `(transient-define-prefix nvp-gud-menu ()
           "GUD"
           :info-manual "Info (debugger)"
           ,@menu
           (interactive)
           (transient-setup 'nvp-gud-menu)
           ;; (if (fboundp 'gud-minor-mode)
           ;;     (transient-setup 'nvp-gud-menu)
           ;;   (user-error "GUD not loaded"))
           )))))

;;;###autoload(autoload 'nvp-gud-menu "nvp-debug" nil t)
(nvp:def-gud-menu)

(provide 'nvp-debug)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-debug.el ends here
