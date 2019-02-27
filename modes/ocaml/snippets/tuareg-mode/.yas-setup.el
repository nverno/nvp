(eval-when-compile
  (defvar yas-text))
(declare-function yas-define-snippets "yasnippet")

;; insert ';;' at end of line if not there
(defsubst ocaml-yas-end ()
  (save-excursion
    (end-of-line)
    (and (not (looking-back ";" 1))
         (insert ";;"))))
