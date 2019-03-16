;;; nvp-compile.el --- compile autoloads -*- lexical-binding: t; -*-

;; Last modified: <2019-03-16 05:39:50>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 12 February 2019

;;; Commentary:
;; Possible packages:
;; - https://github.com/plandes/flex-compile
;; More generalization is good
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(declare-function xterm-color-colorize-buffer "xterm-color")
(autoload 'ansi-color-apply-on-region "ansi-color")

;;;###autoload
(defun nvp-compile (&optional arg)
  "Compile using local `nvp-compile-function' or `nvp-compile-default'.
By default, with single prefix or 3 or more, read compilation command.
With double prefix or more, use comint buffer for compilation."
  (interactive "P")
  (setq current-prefix-arg arg)
  (if (bound-and-true-p nvp-compile-function)
      (call-interactively nvp-compile-function arg)
    (call-interactively #'nvp-compile-default)))

(defun nvp-compile-default (&optional comint read-command)
  "Basic compilation."
  (interactive
   (let ((arg (prefix-numeric-value current-prefix-arg)))
     (list (>= arg 16) (or (eq arg 4) (> arg 16)))))
  (setq-local compilation-read-command read-command)
  (when compilation-read-command
    (setq-local compile-command (compilation-read-command compile-command)))
  (funcall-interactively
   'compile (if (functionp compile-command) (funcall compile-command)
              (eval compile-command))
   comint))

;;;###autoload
(defun nvp-compile-colorize ()
  (interactive)
  (let ((inhibit-read-only t))
    ;; prefer xterm when available
    (if (boundp 'xterm-color-colorize-buffer)
        (xterm-color-colorize-buffer)
     (ansi-color-apply-on-region compilation-filter-start (point-max)))))

(defun nvp-compile-add-local-bindings (buff _stat bindings)
  (with-current-buffer buff
    (dolist (b bindings)
      (local-set-key (kbd (car b)) (cadr b)))))

;;;###autoload
(defun nvp-compile-with-bindings (bindings &rest args)
  "Run basic compile with local BINDINGS in output buffer.
ARGS are passed to `nvp-compile'."
  (let ((compilation-finish-functions
         `(nvp-compile-add-local-bindings buff stat ,bindings)))
    (funcall 'nvp-compile args)))

;; ------------------------------------------------------------
;;; Cmake

;; Run cmake
;; on windows do MSYS makefiles with GNU c++/c compilers from 
;; out-of-source build directory.
;;;###autoload
(defun nvp-compile-cmake (&rest params)
  (interactive)
  (let* ((params (or params (and current-prefix-arg
                                 (read-from-minibuffer "CMake Params: "))))
         (build-dir (make-temp-file "_build" t))
         (args (mapconcat 'identity
                          `(,(file-name-directory buffer-file-name)
                            ;; for MSYS
                            (nvp-with-w32 "-G \"MSYS Makefiles\"")
                            "-DCMAKE_CXX_COMPILER=g++.exe"
                            "-DCMAKE_C_COMPILER=gcc.exe"
                            "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
                            ,@params)
                          " "))
         (default-directory build-dir))
    (async-shell-command (format "cmake %s" args) "*cmake*")))

(provide 'nvp-compile)
;;; nvp-compile.el ends here
