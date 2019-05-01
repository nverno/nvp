;;; nvp-compile.el --- compile autoloads -*- lexical-binding: t; -*-

;;; Commentary:

;; Possible packages:
;; - https://github.com/ReanGD/emacs-multi-compile
;; - https://github.com/plandes/flex-compile -- nah
;; - https://github.com/defunkt/emacs/blob/master/vendor/mode-compile.el
;; - smart-compile
;; - https://github.com/syohex/emacs-quickrun
;; More generalization is good

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(declare-function xterm-color-colorize-buffer "xterm-color")
(autoload 'ansi-color-apply-on-region "ansi-color")

;; FIXME: Obsolete
;; Create compile function, check for makefiles/cmake first, otherwise
;; execute BODY. Prefix argument executes PROMPT-ACTION, and its
;; result is bound to ARGS, which can be used in the body.
(cl-defmacro nvp-make-or-compile-fn
    (name
     (&key
      (doc "Compile using make or cmake if found, otherwise execute body.")
      (make-action
       '(let ((compile-command (or args "make -k")))
          (nvp-compile)))
      (cmake-action
       '(nvp-compile-cmake args))
      (default-prompt
        '(read-from-minibuffer "Compile args: "))
      (prompt-action
       `((cond
          ,@(and cmake-action
                 '((have-cmake
                    (read-from-minibuffer "CMake args: "))))
          ,@(and make-action
                 '((have-make
                    (format "make %s" (read-from-minibuffer "Make args: ")))))
          (t ,default-prompt)))))
     &rest body)
  (declare (indent defun))
  (let ((fn (if (symbolp name) name (intern name))))
    `(progn
       (declare-function nvp-compile "nvp-compile")
       (declare-function nvp-compile-cmake "nvp-compile")
       (defun ,fn (&optional arg)
         ,doc
         (interactive "P")
         (let* (,@(and make-action
                       '((have-make
                          (memq t (mapcar #'file-exists-p
                                          '("Makefile" "makefile" "GNUMakefile"))))))
                ,@(and cmake-action
                       '((have-cmake (file-exists-p "CMakeLists.txt"))))
                (args (and arg ,@(or prompt-action
                                     '((read-from-minibuffer "Compile args: "))))))
           (cond
            ,@(and cmake-action `((have-cmake ,cmake-action)))
            ,@(and make-action `((have-make ,make-action)))
            (t ,@body)))))))

;;;###autoload
(defun nvp-compile (&optional arg)
  "Compile using local `nvp-local-compile-function' or `nvp-compile-default'.
By default, with single prefix or 3 or more, read compilation command.
With double prefix or more, use comint buffer for compilation."
  (interactive "P")
  (setq current-prefix-arg arg)
  (let ((compile-fn (or (bound-and-true-p nvp-local-compile-function)
                        (bound-and-true-p nvp-compile-function)
                        #'nvp-compile-default)))
    (call-interactively compile-fn)))

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

;;;###autoload
(defun nvp-compile-with-bindings (&rest bindings)
  "Run basic compile with local BINDINGS in output buffer.
ARGS are passed to `nvp-compile'."
  (let* ((orig-funcs compilation-finish-functions)
         (lmap (make-sparse-keymap))
         (setup-func
          #'(lambda (&rest _ignored)
              (set-keymap-parent lmap (current-local-map))
              (use-local-map lmap)
              (setq compilation-finish-functions orig-funcs))))
    (pcase-dolist (`(,k . ,b) bindings)
      (define-key lmap (if (vectorp k) k (kbd k)) b))
    (setq compilation-finish-functions setup-func)
    (call-interactively 'nvp-compile)))

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
                            ,(nvp-with-w32 "-G \"MSYS Makefiles\"")
                            "-DCMAKE_CXX_COMPILER=g++"
                            "-DCMAKE_C_COMPILER=gcc"
                            "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
                            ,@params)
                          " "))
         (default-directory build-dir))
    (async-shell-command (format "cmake %s" args) "*cmake*")))

;; -------------------------------------------------------------------
;;; Related commands

;; TODO: display compiler info
;;;###autoload
(defun nvp-compile-help ()
  (interactive))

(provide 'nvp-compile)
;;; nvp-compile.el ends here
