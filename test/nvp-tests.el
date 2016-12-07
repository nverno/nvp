(require 'nvp)
(require 'ert)

(defmacro nvp--should (from to)
  `(with-temp-buffer
     (let ()
       (insert ,from)
       (should (string= (buffer-substring-no-properties (point-min) (point-max))
                      ,to)))))

(defun nvp--run-tests ()
  (interactive)
  (if (featurep 'ert)
      (ert-run-tests-interactively "nvp--test")
    (message "cant run without ert.")))

(provide 'nvp-tests)
