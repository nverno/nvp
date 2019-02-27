(require 'java-tools)
(require 'ert)

(defun java--run-tests ()
  (interactive)
  (if (featurep 'ert)
      (ert-run-tests-interactively "java--test")
    (message "cant run without ert.")))

(provide 'java-tests)
