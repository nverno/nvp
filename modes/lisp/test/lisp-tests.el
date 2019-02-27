(require 'nvp-lisp)
(require 'ert)

(defmacro lisp--should- (from to)
  `(with-temp-buffer
     (let ()
       (insert ,from)
       ;; (indent-region (point-min) (point-max))
       (should (string= (buffer-substring-no-properties (point-min) (point-max))
                      ,to)))))

(defun lisp--run-tests ()
  (interactive)
  (if (featurep 'ert)
      (ert-run-tests-interactively "lisp--test")
    (message "cant run without ert.")))

(provide 'lisp-tests)
