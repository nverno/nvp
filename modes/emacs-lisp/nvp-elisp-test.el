;;; nvp-elisp-test.el --- elisp tests -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-project)
(require 'nvp-test)
(nvp-decls)

;; -------------------------------------------------------------------
;;; Util 

(defun nvp-elisp--run-tests (&optional prefix regexp)
  (eval-buffer)
  (ert-run-tests-interactively
   (or (and prefix (format "%s--test" prefix))
       regexp
       ".*")))

;; -------------------------------------------------------------------
;;; Commands 

;; find and run associated ert tests
;;;###autoload
(defun nvp-elisp-run-tests (arg)
  (interactive "P")
  (nvp-with-project (:test-re ".*tests?\.el")
    (nvp-with-test 'local 'create nil nil nil
      (nvp-elisp--run-tests nil (if arg (read-string "Test regexp: "))))))

(provide 'nvp-elisp-test)
;;; nvp-elisp-test.el ends here
