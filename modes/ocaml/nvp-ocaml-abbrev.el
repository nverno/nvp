;;; nvp-ocaml-abbrev.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))

;; get functions and type signatures for MODULE
(defun nvp-ocaml-abbrev-module-sig (module)
  (with-temp-buffer
    (let ((proc (start-process "ocaml" (current-buffer)
                               "opam" "config" "exec" "--" "ocaml"))
          res)
      (process-send-string
       proc (format "module type S = module type of %s;;\n" module))
      (sit-for 0.1)
      (goto-char (point-min))
      (when (re-search-forward "^\\s-*sig" nil 'move)
        (forward-line)
        (while (not (eobp))
          (when (looking-at
                 (nvp-concat
                  "[ \t]*\\(?:[[:alnum:]]+\\)[ \t]+\\([A-Za-z_0-9]+\\)"
                  "[ \t]*:[ \t]*\\([^\n\r]+\\)$"))
            (push (cons (match-string 1) (match-string 2)) res))
          (forward-line)))
      res)))

(provide 'nvp-ocaml-abbrev)
;;; nvp-ocaml-abbrev.el ends here
