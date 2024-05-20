;;; nvp-c++-test.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-c-compile))
(nvp:req 'nvp-c 'subrs)


(defun nvp-c++-test-init ()
  (insert "boost_init")
  (call-interactively 'yas-expand))

(defvar boost-test-abbrev-table)
;; If new expand template for new test
(defun nvp-c++-test-setup-buffer (&optional new)
  (nvp:c++-test--setup-buffer)
  (when (or current-prefix-arg new)
    (goto-char (point-max))
    (insert "\nbatc")
    (call-interactively 'yas-expand)))

(defun nvp-c++-test-help ()
  (interactive)
  (browse-url "https://github.com/jsankey/boost.test-examples/"))

(nvp:c-define-test-runner-fn nvp-c++-test-run-unit-test 'c++
  ;; flags
  "-std=c++20 -O3 -s"
  ;; link
  "-lboost_unit_test_framework")

(provide 'nvp-c++-test)
;;; nvp-c++-test.el ends here
