(require 'sml-tools)
(require 'ert)

(defmacro sml--should- (from to)
  `(with-temp-buffer
     (let ()
       (insert ,from)
       ;; (indent-region (point-min) (point-max))
       (should (string= (buffer-substring-no-properties (point-min) (point-max))
                      ,to)))))

(defun sml--run-tests ()
  (interactive)
  (if (featurep 'ert)
      (ert-run-tests-interactively "sml--test")
    (message "cant run without ert.")))

(provide 'sml-tests)
