;;; nvp-c-install.el --- install -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-c-ct "./compile/nvp-c-ct"))
(require 'nvp-c)
(nvp-decls)

(nvp-package-define-root :name nvp-c)

;; generate site-specific include paths, flags, etc.
(defun nvp-c-gen-site-vars (&optional arg)
  (let ((includes (expand-file-name "nvp-c-vars.el" nvp-c--dir))
        (prog (expand-file-name "emacs/gen-c-vars" nvp/bin)))
    (when (or arg
              (not (file-exists-p includes))
              (nvp-file-older-than-days includes 20))
      (nvp-with-process "bash"
        :proc-buff " *c-vars*"
        :proc-args (prog "-o" includes)
        :callback (lambda (p _m)
                    (when (zerop (process-exit-status p))
                      (load includes)
                      (message "loaded %s" includes))
                    (kill-buffer (process-buffer p)))))))

;; make includes.el and install dependencies or dont with NODEPS
;; force includes.el refresh with ARG
;;;###autoload
(defun nvp-c-install (command &optional force)
  (interactive (list 'vars current-prefix-arg))
  (cl-case command
    (vars (nvp-c-gen-site-vars force))
    (irony (nvp-c-install-irony))
    (t)))

;;; Irony server

(nvp-with-gnu
  (defun nvp-c-install-irony ()
    (require 'irony)
    (call-interactively 'irony-install-server)))

(nvp-with-w32
  ;; Install irony server using MSYS compilers. Return process object
  (defun nvp-c-install-irony (&optional irony-prefix irony-dir build-cmd)
    (require 'irony)
    (or irony-prefix (setq irony-prefix irony-server-install-prefix))
    (unless (file-exists-p irony-server-install-prefix)
      (let* ((irony-dir (or irony-dir
                            (expand-file-name "server"
                                              (file-name-directory
                                               (locate-library "irony")))))
             (build-dir (make-temp-file "_build" t))
             (irony-prefix (or irony-prefix
                               (expand-file-name ".emacs.d/cache/irony" "~")))
             (args (mapconcat 'identity
                              `(,irony-dir
                                "-G \"MSYS Makefiles\""
                                "-DCMAKE_CXX_COMPILER=g++.exe"
                                "-DCMAKE_C_COMPILER=gcc.exe"
                                ,(concat "-DCMAKE_INSTALL_PREFIX=" irony-prefix))
                              " "))
             (build-cmd
              "cmake --build . --use-stderr --config Release --target install")
             (default-directory build-dir))
        (start-process-shell-command
         "cmake" "*nvp-install*" (format "cmake %s && %s" args build-cmd))))))

(provide 'nvp-c-install)
;;; nvp-c-install.el ends here
