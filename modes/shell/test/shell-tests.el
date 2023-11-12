;; -*- lexical-binding: t; -*-
(require 'nvp-shell)
(require 'ert)

(defmacro shell--should-indent (from to)
  `(with-temp-buffer
     (let ()
       (sh-mode)
       (insert ,from)
       (should (string= (buffer-substring-no-properties (point-min) (point-max))
                      ,to)))))

(defun shell--run-tests ()
  (interactive)
  (if (featurep 'ert)
      (ert-run-tests-interactively "shell--test")
    (message "cant run without ert.")))

(provide 'shell-tests)
