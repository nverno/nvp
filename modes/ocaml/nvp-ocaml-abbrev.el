;;; nvp-ocaml-abbrev.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;; can just get module symbols from ocaml-help.el
;; eg. (ocaml-module-symbols (assoc "List" ocaml-module-alist))
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-ocaml)

(defun nvp-ocaml--parse-sigs ()
  (goto-char (point-min))
  (let (res)
    (when (re-search-forward "^\\s-*sig" nil 'move)
      (forward-line)
      (while (not (eobp))
        (when (looking-at
               (nvp:concat
                "[ \t]*\\(?:[[:alnum:]]+\\)[ \t]+\\([A-Za-z_0-9]+\\)"
                "[ \t]*:[ \t]*\\([^\n\r]+\\)$"))
          (push (cons (match-string 1) (match-string 2)) res))
        (forward-line)))
    res))

;; get functions and type signatures for MODULE
(defun nvp-ocaml-abbrev-module-sig (module)
  (let
      ((proc
        (nvp-with-process "opam"
          :proc-buff "*ocaml-sigs*"
          :proc-args ("config" "exec" "--" "ocaml"
                      "-init" (expand-file-name ".ocamlinit-bare" nvp-ocaml--etc))
          :callback
          (lambda (p _m)
            (if (not (zerop (process-exit-status p)))
                (progn (message "failed!")
                       (pop-to-buffer (process-buffer p)))
              (with-current-buffer (process-buffer p)
                (prog1 (nvp-ocaml--parse-sigs)
                  (kill-this-buffer))))))))
    (process-send-string
     proc (format "module type S = module type of %s;;\n" module))))

(provide 'nvp-ocaml-abbrev)
;;; nvp-ocaml-abbrev.el ends here
