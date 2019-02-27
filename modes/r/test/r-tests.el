(require 'nvp-r)
(require 'ert)

(defmacro r--should (from to)
  `(with-temp-buffer
     (let ()
       ;; (ess-mode)
       (insert ,from)
       (should (string= (buffer-substring-no-properties (point-min) (point-max))
                        ,to)))))

(defun r--run-tests ()
  (interactive)
  (if (featurep 'ert)
      (ert-run-tests-interactively "r--test")
    (message "cant run without ert.")))

(provide 'r-tests)
