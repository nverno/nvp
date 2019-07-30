;;; nvp-c-test.el --- C tests -*- lexical-binding: t; -*-
;;; Commentary:

;;; TODO:
;; - Generate test abbrevs from macros in header files
;; - hideif

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (defvar check-abbrev-table)
  (defvar cunit-abbrev-table)
  (defvar unity-abbrev-table)
  (defvar nvp-abbrev-local-table))
(require 'nvp-test)
(require 'nvp-c)
(autoload 'yas-expand "yasnippet")
(declare-function clang-complete-create-or-update "clang-complete")

;; function to run unit test from test buffer
(defvar nvp-c-test-runner 'nvp-c-test-default-runner)

;; -------------------------------------------------------------------
;;; Util

;; assume first path will be root, eg ~/.local/include:etc
(eval-and-compile
  (defun nvp-c-local-include-path (path)
   (expand-file-name
    path
    (car (split-string (or (getenv "C_INCLUDE_PATH")
                           (getenv "CPATH")) path-separator t " ")))))

(eval-when-compile
  ;; locally set keys in test buffers to run tests
  (defmacro nvp-c-test--buffer (type)
    `(progn
       (setq-local local-abbrev-table
                   (symbol-value (intern (concat ,type "-abbrev-table"))))
       (setq-local nvp-abbrev-local-table ,type)
       (nvp-set-local-keymap :use t
         ("C-c C-c" . nvp-c-test-run-unit-test))))

  ;; generate function to run unit tests
  ;; Runs tests in current buffer or FILE if non-nil
  (defmacro nvp-c-test--runner-fn (name &optional c++ flags libs)
    (declare (indent defun))
    (let ((fn (nvp-string-or-symbol name)))
      `(progn
         ;; (,'declare-function ,fn "")
         (defun ,fn (save &optional file post-compile)
          "Run tests in current buffer or FILE. Don't throw away executable if KEEP
is non-nil."
          (interactive "P")
          (let* ((default-directory (if file (file-name-directory file)
                                      default-directory))
                 (out (nvp-c-out-file file))
                 (compile-command
                  (concat
                   (nvp-concat
                    (nvp-program ,(if c++ "g++" "gcc")) " " ,flags " ")
                   " -o " out " " (or file buffer-file-name)
                   (nvp-concat " " ,libs ";")
                   (or save post-compile
                       (concat "./" (file-name-nondirectory out)
                               "; rm " out))))
                 (compilation-read-command nil))
            (call-interactively 'compile)))))))

;; -------------------------------------------------------------------
;;; Setup Tests

(eval-when-compile
  (defvar yas-selected-text))
(declare-function yas-expand-snippet "yasnippet")
(declare-function yas-lookup-snippet "yasnippet")

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
  (nvp-c-test--buffer type))

;;;###autoload(autoload 'nvp-project-c-unity-setup "nvp-c-test")
(nvp-define-project c-unity
  :test-fmt "test_%s"
  :test-init-function (apply-partially 'nvp-c-test-init "unity")
  :test-buffer-function (apply-partially 'nvp-c-test-buffer
                                         "unity" 'nvp-c-test-run-unity-test)
  :test-run-unit-function 'nvp-c-test-run-unity-test)

(nvp-define-project c-check
  :test-fmt "test_%s"
  :test-init-function (apply-partially 'nvp-c-test-init "check")
  :test-buffer-function (apply-partially 'nvp-c-test-buffer "check")
  :test-run-unit-function 'nvp-c-test-default-runner)

(nvp-define-project c-cunit
  :test-fmt "test_%s"
  :test-init-function (apply-partially 'nvp-c-test-init "cunit")
  :test-buffer-function (apply-partially 'nvp-c-test-buffer "cunit")
  :test-run-unit-function 'nvp-c-test-default-runner)

;; -------------------------------------------------------------------
;;; Commands

(defun nvp-c-test-run-unit-test (arg)
  (interactive "P")
  (funcall-interactively nvp-c-test-runner arg))

;; commands to run unit test
(nvp-c-test--runner-fn nvp-c-test-default-runner nil
  "-Werror -Wall -std=c11 -O2 -s -DTEST -I.")

(nvp-c-test--runner-fn nvp-c-test-run-unity-test nil
  (concat
   "-Werror -Wall -std=c11 -O2 -s -DTEST -I. -I"
   (nvp-c-local-include-path "unity/src") " "
   (nvp-c-local-include-path "unity/src/unity.c")))

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
   file (concat "valgrind " (nvp-c-out-file file))))

;;;###autoload
(defun nvp-c-test-help-online ()
  (interactive)
  (browse-url "https://github.com/ThrowTheSwitch/Unity/blob/master/docs/UnityAssertionsCheatSheetSuitableforPrintingandPossiblyFraming.pdf")
  ;; (browse-url "https://libcheck.github.io/check/index.html")
  )

(provide 'nvp-c-test)
;;; nvp-c-test.el ends here
