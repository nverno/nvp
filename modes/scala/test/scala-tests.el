(require 'scala-tools)
(require 'ert)

(defun scala--run-tests ()
  (interactive)
  (if (featurep 'ert)
      (ert-run-tests-interactively "scala--test")
    (message "cant run without ert.")))

(provide 'scala-tests)
