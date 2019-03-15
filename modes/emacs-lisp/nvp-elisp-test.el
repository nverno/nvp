;;; nvp-elisp-test.el --- elisp tests -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/elisp-utils
;; Last modified: <2019-03-15 09:24:59>
;; Created:  2 December 2016

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'nvp-project)
(require 'nvp-test)
(declare-function ert-run-tests-interactively "ert")

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
