;;; nvp-c-test.el --- C tests -*- lexical-binding: t; -*-
;;; Commentary:

;;; TODO:
;; - Generate test abbrevs from macros in header files
;; - hideif

;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-test)
(require 'nvp-c)
(nvp:req 'nvp-c 'subrs)
(nvp:decls :p (clang yas))
(nvp:auto "yasnippet" 'yas-expand)

;; function to run unit test from test buffer
(defvar nvp-c-test-runner #'nvp-c-test-default-runner)

;; -------------------------------------------------------------------
;;; Setup Tests

;; init new test file
(defun nvp-c-test-init (type &optional source-file)
  (nvp-c-setenv type)
  ;; call after setting test environment to get paths to unit testing
  ;; framework included
  (clang-complete-create-or-update nil 'c-mode '(("-D" . "TEST")))
  (and (fboundp 'irony-cdb-autosetup-compile-options)
       (irony-cdb-autosetup-compile-options))
  (yas-expand-snippet
   (yas-lookup-snippet (concat type "_init") 'c-mode)
   nil nil
   `((include-file ,(file-relative-name source-file)))))

;; called when opening a test buffer
(defun nvp-c-test-buffer (type &optional runner)
  (setq-local nvp-c-test-runner runner)
  (nvp:c-test--buffer type))

;; -------------------------------------------------------------------
;;; Commands

(defun nvp-c-test-run-unit-test (arg)
  (interactive "P")
  (funcall-interactively nvp-c-test-runner arg))

;; commands to run unit test
(nvp:c-define-test-runner-fn nvp-c-test-default-runner nil
  "-Werror -Wall -std=c11 -O2 -s -DTEST -I.")

(nvp:c-define-test-runner-fn nvp-c-test-run-unity-test nil
  (concat
   "-Werror -Wall -std=c11 -O2 -s -DTEST -I. -I"
   (nvp:c-local-include-path "unity/src") " "
   (nvp:c-local-include-path "unity/src/unity.c")))

;; compile associated unit test and run valgrind on it
;;;###autoload
(defun nvp-c-test-run-valgrind (file)
  (interactive
   (list (if (nvp-test-file-p)
             (buffer-file-name)
           (or (ignore-errors
                 (nvp-test-find-matching-test
                  (buffer-file-name) (nvp-test-dir 'local)))
               (buffer-file-name)))))
  (funcall-interactively
   nvp-test-run-unit-function current-prefix-arg
   file (concat "valgrind " (nvp:c-out-file file))))

;; ;;;###autoload
;; (defun nvp-c-test-help-online ()
;;   (interactive)
;;   (browse-url "https://github.com/ThrowTheSwitch/Unity/blob/master/docs/UnityAssertionsCheatSheetSuitableforPrintingandPossiblyFraming.pdf")
;;   ;; (browse-url "https://libcheck.github.io/check/index.html")
;;   )

(provide 'nvp-c-test)
;;; nvp-c-test.el ends here
