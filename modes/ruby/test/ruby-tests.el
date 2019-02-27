(require 'nvp-ruby)
(require 'ert)

(defmacro ruby--should- (from to)
  `(with-temp-buffer
     (let ()
       (insert ,from)
       ;; (indent-region (point-min) (point-max))
       (should (string= (buffer-substring-no-properties (point-min) (point-max))
                      ,to)))))

(defun ruby--run-tests ()
  (interactive)
  (if (featurep 'ert)
      (ert-run-tests-interactively "ruby--test")
    (message "cant run without ert.")))

(provide 'ruby-tests)
