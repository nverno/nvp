;;; nvp-rspec.el --- RSpec -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:

(defconst nvp-rspec-font-lock-keywords
  `((,(regexp-opt
       '("expect" "describe" "it" "context" "before"
         "feature" "scenario")
       'symbols)
     (1 font-lock-function-name-face))))

;;;###autoload
(defun nvp-rspec-font-lock ()
  (font-lock-add-keywords 'ruby-mode nvp-rspec-font-lock-keywords))

(provide 'nvp-rspec)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-rspec.el ends here
