;;; nvp-c-test.el --- C tests -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-compile))
(nvp:req 'nvp-c 'subrs)
(require 'nvp-test)
(nvp:decls :p (clang yas))
(nvp:auto "yasnippet" 'yas-expand)


;; function to run unit test from test buffer
(defvar nvp-c-test-runner #'nvp-c-test-default-runner)

;; Assume first path will be root, eg ~/.local/include:etc
(defsubst nvp-c--local-include-path (path)
  (expand-file-name
   path (car (split-string (or (getenv "C_INCLUDE_PATH")
                               (getenv "CPATH"))
                           path-separator t " "))))

;;; Environment
(defvar nvp-c-ext-includes
  '(("unity" (expand-file-name ".local/include/unity/src" (getenv "HOME"))
     "/unity/src")
    ("R"     (expand-file-name "lib/R/include" (asdf-where "R")) "/R/include")
    ("emacs" emacs-src-dir "/emacs/src"))
  "Paths to external includes.")

(defvar c/R-abbrev-table)
;; set environment stuff for macro expanding
;; could also set local `c-macro-preprocessor'?
;;;###autoload
(defun nvp-c-setenv (type)
  "Add include path of TYPE to macroexpand stuff."
  (interactive
   (list (completing-read "Add include path for: " nvp-c-ext-includes)))
  (cl-destructuring-bind (kind loc regex) (assoc-string type nvp-c-ext-includes)
    (pcase kind
      (`"R"
       (setq-local local-abbrev-table c/R-abbrev-table)
       (setq-local nvp-local-abbrev-table "c/R"))
      (_ nil))
    (nvp-env-add "C_INCLUDE_PATH" (eval loc) regex)))

;; -------------------------------------------------------------------
;;; Setup Tests
(nvp:decl clang-complete-create-or-update)

;; Init new test file
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

;; Called when opening a test buffer
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
   (nvp-c--local-include-path "unity/src") " "
   (nvp-c--local-include-path "unity/src/unity.c")))

;;;###autoload
(defun nvp-c-test-run-valgrind (file)
  "Compile associated unit test and run valgrind on it."
  (interactive
   (list (if (nvp-test-file-p)
             (buffer-file-name)
           (or (ignore-errors
                 (nvp-test-find-matching-test
                  (buffer-file-name) (nvp-test-dir 'local)))
               (buffer-file-name)))))
  (funcall-interactively
   nvp-test-run-unit-function current-prefix-arg
   file (concat "valgrind " (nvp-c--out-file file))))

(provide 'nvp-c-test)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-c-test.el ends here
