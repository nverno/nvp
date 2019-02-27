(require 'ocaml-tools)
(require 'ert)

(defmacro ocaml--should-indent (from to)
  `(with-temp-buffer
     (let ()
       (insert ,from)
       (should (string= (buffer-substring-no-properties (point-min) (point-max))
                      ,to)))))

(defun ocaml--run-tests ()
  (interactive)
  (if (featurep 'ert)
      (ert-run-tests-interactively "ocaml--test")
    (message "cant run without ert.")))

(provide 'ocaml-tests)
