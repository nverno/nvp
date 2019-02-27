(require 'nvp-scheme)
(require 'ert)

(defmacro scheme--should- (from to)
  `(with-temp-buffer
     (let ()
       (insert ,from)
       ;; (indent-region (point-min) (point-max))
       (should (string= (buffer-substring-no-properties (point-min) (point-max))
                      ,to)))))

(defun scheme--run-tests ()
  (interactive)
  (if (featurep 'ert)
      (ert-run-tests-interactively "scheme--test")
    (message "cant run without ert.")))

(provide 'scheme-tests)
