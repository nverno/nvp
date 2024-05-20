;;; nvp-c-compile.el --- C compile functions -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-compile))
(nvp:decls)


(defvar nvp-c-address-error-regexp
  '(address-sanitizer
    "^\\s-*#[0-9]+ [x[:xdigit:]]+ in [^\n]* \\([^\n:]+\\):\\([0-9]+\\)" 1 2)
  "Compilation error regexp entry to match address sanitizer stack traces.")

(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist 'address-sanitizer t)
  (add-to-list 'compilation-error-regexp-alist-alist nvp-c-address-error-regexp t))

(defvar nvp-c-compiler (nvp:program "gcc"))
(defvar nvp-c++-compiler (nvp:program "g++"))

;;; Compile flags
(defvar nvp-c-compile-extra-flags nil)
(defvar nvp-c-compile-defines nil "List of defines to add to compile command.")
(defvar nvp-c-compile-opt-flag "-O2")
(defvar nvp-c-compile-std-flag (cons "-std=c11" "-std=c++20"))
(defvar nvp-c-compile-debug-flags "-ggdb3")
(defvar nvp-c-compile-feature-flags "-fsanitize=address -fsanitize=undefined")
;; First entry applies to bot c/c++, second to c only
(defvar nvp-c-compile-warning-flags
  (cons "-Wall -Wextra -Wpedantic \
-Wformat=2 -Wno-unused-parameter -Wshadow \
-Wwrite-strings -Wredundant-decls -Wmissing-include-dirs"
        "-Wstrict-prototypes -Wold-style-definition -Wnested-externs"))
(defvar nvp-c-gcc-flags (cons "-Wlogical-op" "-Wjump-misses-init")
  "GCC specific compiler flags.")

(defsubst nvp-c--by-mode (c-option cpp-option)
  (if (derived-mode-p 'c++-mode)
      cpp-option
    c-option))

(defsubst nvp-c--out-file (&optional file)
  (concat (nvp:no-ext file) (nvp:with-gnu/w32 ".out" ".exe")))

(eval-when-compile
  (defsubst nvp:c-compiler ()
    (nvp-c--by-mode nvp-c-compiler nvp-c++-compiler))

  (defsubst nvp:c-compiler-p (compiler compilers)
    (cl-member (file-name-base compiler) compilers :test #'string=)))

(defsubst nvp-c--warning-flags (compiler)
  (let ((c-only-p (nvp:c-compiler-p compiler '("gcc" "clang")))
        (gcc-p (nvp:c-compiler-p compiler '("gcc" "g++"))))
    (list (car nvp-c-compile-warning-flags)
          (and c-only-p (cdr nvp-c-compile-warning-flags))
          (and gcc-p (car nvp-c-gcc-flags))
          (and gcc-p c-only-p (cdr nvp-c-gcc-flags)))))

(cl-defun nvp-c-compile-flags
    (compiler &key
              (defines nvp-c-compile-defines)
              (debug-flags nvp-c-compile-debug-flags)
              (opt-flag nvp-c-compile-opt-flag)
              (std-flag (nvp-c--by-mode (car nvp-c-compile-std-flag)
                                        (cdr nvp-c-compile-std-flag)))
              (feature-flags nvp-c-compile-feature-flags)
              (extra-flags nvp-c-compile-extra-flags)
              (warnings t)
              &allow-other-keys)
  "Return C compile flags for COMPILER."
  (or (getenv "CFLAGS")
      (let* ((defs (when defines
                     (concat
                      "-D" (mapconcat 'identity (seq-uniq defines) " -D"))))
             (flags (append
                     (list opt-flag std-flag debug-flags defs extra-flags
                           feature-flags)
                     (and warnings (nvp-c--warning-flags compiler)))))
        (mapconcat #'identity (delq nil flags) " "))))

(cl-defun nvp-c-compile-build-command
    (&rest args &key compiler srcfile outfile flags &allow-other-keys)
  "Return C compile command."
  (--if-let (assoc-default 'compile-command file-local-variables-alist)
      (cons it (or (assoc-default 'nvp-target file-local-variables-alist)
                   (nvp-c--out-file
                    (or srcfile (file-name-nondirectory (buffer-file-name))))))
    (or (assoc-default 'compile-command file-local-variables-alist)
        (let* ((compiler (or compiler (nvp:c-compiler)))
               (src (or srcfile (file-name-nondirectory buffer-file-name)))
               (out (or outfile (nvp-c--out-file src)))
               (flags (or flags (apply #'nvp-c-compile-flags compiler args))))
          (cons (format "%s %s -o %s %s" compiler flags out src) out)))))

;;;###autoload
(defun nvp-c-compile (prefix)
  "Compile using file-local variables, make, cmake, or build compile command."
  (interactive "P")
  (unless (nvp-compile-maybe-default prefix)
    (let ((compile-command (car (nvp-c-compile-build-command))))
      (call-interactively #'nvp-compile))))

;;;###autoload
(defun nvp-c-compile-and-run (prefix &optional keep post-action &rest args)
  "Compile current file and run it with output to compilation buffer.
Pass PREFIX to `nvp-compile'."
  (interactive (list current-prefix-arg))
  (let* ((cmd (apply #'nvp-c-compile-build-command args))
         (build-cmd (car cmd))
         (out (cdr cmd))
         (command (concat build-cmd "; "
                          (or (and (eq post-action 'no-run) "")
                              post-action (concat "./" out))
                          (unless keep (concat "; rm " out)))))
    (setq-local compile-command command)
    (funcall-interactively 'nvp-compile prefix)))

;;;###autoload
(defun nvp-c-compile-watch (error-file)
  "Compile and watch error output in ERROR-FILE."
  (interactive
   (let ((default (make-temp-file "stderr")))
     (list (if current-prefix-arg
               (read-file-name "Output file: " nil default)
             default))))
  (let* ((src (file-name-nondirectory (buffer-file-name)))
         (out (nvp-c--out-file src))
         (post (concat "./" out " 2> " error-file
                       "& gnome-terminal -- watch tail -n10 " error-file
                       "; wait")))
    (nvp-c-compile-and-run current-prefix-arg nil post
                           :srcfile src :outfile out :defines '("TEST"))))

;;;###autoload
(defun nvp-c-compile-debug (prefix)
  "Compile with debug flags and run `gdb'.
PREFIX is passed to `nvp-compile'."
  (interactive "P")
  (nvp-c-compile-and-run prefix t 'no-run :defines '("DEBUG"))
  (call-interactively 'gdb))

;;;###autoload
(defun nvp-c-compile-asm ()
  "Show assembly in other window.
Ddelete assembly output when assembly buffer is killed."
  (interactive)
  (let* ((asm-file (concat (file-name-sans-extension buffer-file-name) ".s"))
         (cmd (nvp-c-compile-build-command
               :opt-flag "-Og" :extra-flags "-S" :outfile asm-file))
         (compile-command (car cmd)))
    (with-current-buffer (call-interactively 'nvp-compile)
      (pop-to-buffer (current-buffer))
      (let ((asm-file asm-file))
        (add-hook 'compilation-finish-functions
                  (lambda (_b _s)
                    (find-file-other-window asm-file)
                    (add-hook 'kill-buffer-hook
                              (lambda () (delete-file buffer-file-name)) nil 'local))
                  nil 'local)))))

;; (autoload 'gdb-disassembly-mode "gdb-mi")
(nvp:decl objdump-mode)
;;;###autoload
(defun nvp-c-compile-objdump ()
  "Dump objects in compilation buffer, setup imenu for function jumps."
  (interactive)
  (let* ((obj-file (concat (file-name-sans-extension (buffer-file-name)) ".o"))
         (cmd (nvp-c-compile-build-command
               :opt-flag "-Og" :extra-flags "-c" :outfile obj-file))
         (compile-command (concat (car cmd)
                                  "; objdump -d " obj-file
                                  "; rm " obj-file))
         compilation-scroll-output)
    (with-current-buffer (call-interactively 'nvp-compile)
      (pop-to-buffer (current-buffer))
      (add-hook 'compilation-finish-functions
                (lambda (_b _s)
                  ;; XXX(5/20/24): add tab/backtab movement
                  (objdump-mode)
                  (setq-local imenu-generic-expression
                              '((nil "^[0-9]+ <\\([^>]+\\)>:" 1)))
                  (search-forward "Disassembly" nil 'move 1))
                nil 'local))))

;;;###autoload
(defun nvp-c-compile-strace ()
  "Compile and run strace on output in compilation buffer."
  (interactive)
  (let* ((cmd (nvp-c-compile-build-command :opt-flag "-Og"))
         (prog (cdr cmd))
         (strace-file (concat (file-name-sans-extension prog) ".strace"))
         (compile-command
          (concat (car cmd) "; strace -o " strace-file " ./" prog))
         compilation-scroll-output)
    (with-current-buffer (call-interactively 'nvp-compile)
      (pop-to-buffer (current-buffer))
      (let ((strace-file strace-file))
        (add-hook 'compilation-finish-functions
                  (lambda (_b _s)
                    (find-file-other-window strace-file)
                    (add-hook 'kill-buffer-hook
                              (lambda () (delete-file buffer-file-name)) nil 'local))
                  nil 'local)))))

(provide 'nvp-c-compile)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-c-compile.el ends here
