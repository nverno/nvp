;;; nvp-c++-test.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-c-macros "macs/nvp-c-macros"))
(require 'nvp-c)
(require 'nvp-test)
(autoload 'yas-expand "yasnippet")
(defvar boost-test-abbrev-table)

;; -------------------------------------------------------------------
;;; Setup Test

(defun nvp-c++-test-init ()
  (insert "boost_init")
  (call-interactively 'yas-expand))

;; if new expand template for new test
(defun nvp-c++-test-setup-buffer (&optional new)
  (nvp-c++-test--setup-buffer)
  (when (or current-prefix-arg new)
    (goto-char (point-max))
    (insert "\nbatc")
    (call-interactively 'yas-expand)))

;;;###autoload(autoload 'nvp-project-c++-boost-setup "nvp-c++-test")
(nvp-define-project c++-boost
  :test-fmt "test_%s"
  :test-init-function 'nvp-c++-test-init
  :test-buffer-function 'nvp-c++-test-setup-buffer
  :test-run-unit-function 'nvp-c++-test-run-unit-test)

;; -------------------------------------------------------------------
;;; Commands 

(defun nvp-c++-test-help ()
  (interactive)
  (browse-url "https://github.com/jsankey/boost.test-examples/"))

(nvp-c-test--runner-fn nvp-c++-test-run-unit-test 'c++
  ;; flags
  "-std=c++14 -O3 -s"
  ;; link
  "-lboost_unit_test_framework")

(provide 'nvp-c++-test)
;;; nvp-c++-test.el ends here
