;;; nvp-c-subrs.el --- compile-time -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'nvp-macro)
(defvar check-abbrev-table)
(defvar cunit-abbrev-table)
(defvar unity-abbrev-table)
(defvar nvp-abbrev-local-table)
(defvar boost-test-abbrev-table)

;; -------------------------------------------------------------------
;;; Basic utils

(defsubst nvp-c-out-file (&optional file)
  (concat (nvp:no-ext file) (nvp:with-gnu/w32 ".out" ".exe")))

;; associated header file name
(defsubst nvp-c--header-file-name (&optional buffer ext)
  (concat (nvp:no-ext buffer) (or ext ".h")))

;; assume first path will be root, eg ~/.local/include:etc
(defsubst nvp-c-local-include-path (path)
  (expand-file-name
   path
   (car (split-string (or (getenv "C_INCLUDE_PATH")
                          (getenv "CPATH")) path-separator t " "))))

;; -------------------------------------------------------------------
;;; Tests

;; locally set keys in test buffers to run tests
(defmacro nvp-c-test--buffer (type)
  `(progn
     (setq-local local-abbrev-table
                 (symbol-value (intern (concat ,type "-abbrev-table"))))
     (setq-local nvp-abbrev-local-table ,type)
     (nvp:set-local-keymap :use t
       ("C-c C-c" . nvp-c-test-run-unit-test))))

;; generate function to run unit tests
;; Runs tests in current buffer or FILE if non-nil
(defmacro nvp-c-test--runner-fn (name &optional c++ flags libs)
  (declare (indent defun))
  (let ((fn (nvp:as-symbol name)))
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
                  (nvp:concat
                   (nvp:program ,(if c++ "g++" "gcc")) " " ,flags " ")
                  " -o " out " " (or file buffer-file-name)
                  (nvp:concat " " ,libs ";")
                  (or save post-compile
                      (concat "./" (file-name-nondirectory out)
                              "; rm " out))))
                (compilation-read-command nil))
           (call-interactively 'compile))))))

;; -------------------------------------------------------------------
;;; C++

(defmacro nvp-with-c++-vars (&rest body)
  (declare (indent defun))
  `(nvp-with-project
     (:test-re ".*test.*\.cpp" :root '("test" "tests" ".git" ".projectile"))
     ,@body))

(defmacro nvp-c++-test--setup-buffer ()
  `(progn
     (setq-local local-abbrev-table boost-test-abbrev-table)
     (nvp:set-local-keymap :use t
       ("C-c C-c" . nvp-c++-test-run-unit-test))))

(provide 'nvp-c-subrs)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-c-subrs.el ends here
