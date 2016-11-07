(require 'nvp)
(require 'ert)

(defmacro nvp--should (from to)
  `(with-temp-buffer
     (let ()
       (insert ,from)
       ;; (indent-region (point-min) (point-max))
       (should (string= (buffer-substring-no-properties (point-min) (point-max))
                      ,to)))))

(ert-deftest nvp--test-program ()
  "os dependent program expansion"
  (let ((nvp-g++-program "c:/g++"))
    (if (eq system-type 'windows-nt)
        (should (eq nvp-g++-program (nvp-program "g++")))
      (should (string= "g++" (nvp-program "g++"))))))

(defun nvp--run-tests ()
  (interactive)
  (if (featurep 'ert)
      (ert-run-tests-interactively "nvp--test")
    (message "cant run without ert.")))

(provide 'nvp-tests)
