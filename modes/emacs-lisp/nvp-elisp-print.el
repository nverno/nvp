;;; nvp-elisp-print.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'transient)


(nvp:transient-define-vars nvp--elisp
  print-level
  print-length
  print-quoted
  (print-integers-as-characters . t)
  (print-escape-newlines . t)
  (print-escape-control-characters . t)
  (print-escape-nonascii . t)
  (print-escape-multibyte . t)
  eval-expression-print-level
  eval-expression-print-length
  (cl-print-readably . t)
  cl-print-string-length)


;;;###autoload(autoload 'nvp-elisp-print-menu "nvp-elisp-print" nil t)
(transient-define-prefix nvp-elisp-print-menu ()
  "Print settings."
  [["Print"
    ("d" "Level" nvp--elisp-print-level)
    ("l" "Length" nvp--elisp-print-length)
    ("i" "Ints as chars" nvp--elisp-print-integers-as-characters)]
   ["Escape"
    ("n" "Newlines" nvp--elisp-print-escape-newlines)
    ("a" "Non-ascii" nvp--elisp-print-escape-nonascii)
    ("c" "Control chars" nvp--elisp-print-escape-control-characters)
    ("m" "Multi-byte" nvp--elisp-print-escape-multibyte)]
   ["Eval-Expr"
    ("ed" "Level" nvp--elisp-eval-expression-print-level)
    ("el" "Length" nvp--elisp-eval-expression-print-length)]
   ["Cl-Print"
    ("r" "Readably" nvp--elisp-cl-print-readably)
    ("s" "String length" nvp--elisp-cl-print-string-length)]])


(provide 'nvp-elisp-print)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-elisp-print.el ends here
