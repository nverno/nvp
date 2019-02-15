(require 'nvp)
(require 'ert)

(defmacro nvp--should (from to)
  `(with-temp-buffer
     (let ()
       (insert ,from)
       (should (string= (buffer-substring-no-properties (point-min) (point-max))
                      ,to)))))

(provide 'nvp-tests)
