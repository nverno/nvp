;;; nvp-ocamldebug.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'ocamldebug nil t)
(nvp:decls :p (tuareg) :f (ocamldebug))

;; ocamldebug.el has `ocamldebug-prefix-map'
(defvar ocaml-debug-key-prefix (kbd "<f2> d"))

(eval-when-compile
  (defmacro nvp:ocaml-debug-bindings (&rest bindings)
    (declare (indent defun))
    `(with-eval-after-load 'ocamldebug
       (progn
         ,@(cl-loop for (name . k) in bindings
              collect `(define-key tuareg-mode-map
                         (vconcat ocaml-debug-key-prefix ,k)
                         ',(intern (concat "ocamldebug-" name))))))))

(defvar tuareg-mode-map)
(with-eval-after-load 'tuareg
  (nvp:ocaml-debug-bindings
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
    ("step"      . "s")
    ("break"     . "\C-b")))

(defun nvp-ocaml-debug-help ()
  (interactive)
  (browse-url "https://caml.inria.fr/pub/docs/manual-ocaml/debugger.html"))

;; compile and debug
;;;###autoload
(defun nvp-ocaml-compile-and-debug ()
  (interactive)
  (funcall-interactively 'nvp-ocaml-compile '("-g" "-o a.out"))
  (ocamldebug "a.out"))

(provide  'nvp-ocamldebug)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-ocamldebug.el ends here
