;;; nvp-ocaml-debug.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-ocaml)
(require 'ocamldebug)
(nvp-decls :f (ocamldebug))

;; ocamldebug.el doesn't define a prefix key like gud-gdb
(defvar ocaml-debug-key-prefix (kbd "<f2> d"))

(defmacro ocaml-debug-bindings (&rest bindings)
  (declare (indent defun))
  `(with-eval-after-load 'ocamldebug
     (progn
       ,@(cl-loop for (name . k) in bindings
            collect `(define-key tuareg-mode-map
                       (vconcat ocaml-debug-key-prefix ,k)
                       ',(intern (concat "ocamldebug-" name)))))))

(ocaml-debug-bindings
  ("run"       . "\C-r")
  ("reverse"   . "\C-v")
  ("last"      . "\C-l")
  ("backtrace" . "\C-t")
  ("open"      . "\C-o")
  ("close"     . "\C-c")
  ("finish"    . "\C-f")
  ("print"     . "\C-p")
  ("next"      . "\C-n")
  ("up"        . "<")
  ("down"      . ">")
  ("break"     . "\C-b"))

(defun nvp-ocaml-debug-help ()
  (interactive)
  (browse-url "https://caml.inria.fr/pub/docs/manual-ocaml/debugger.html"))

;; compile and debug
;;;###autoload
(defun nvp-ocaml-compile-and-debug ()
  (interactive)
  (funcall-interactively 'nvp-ocaml-compile '("-g" "-o a.out"))
  (ocamldebug "a.out"))

(provide  'nvp-ocaml-debug)
;;; nvp-ocaml-debug.el ends here
