;;; nvp-c-install.el --- install -*- lexical-binding: t; -*-

;;; Commentary:
;; TODO: remove
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-c)
(nvp-decls)

(nvp-package-define-root :name nvp-c)

;; make includes.el and install dependencies or dont with NODEPS
;; force includes.el refresh with ARG
;;;###autoload
(defun nvp-c-install (arg &optional includes irony)
  (interactive "P")
  (let ((arg arg))
    (cond
      (includes 
       ;; write sys include paths to nvp-c-include.el
       (nvp-with-process-log (nvp-c-install-includes arg)
         :on-success (progn
                       (load (expand-file-name "nvp-c-include" nvp-c--dir))
                       (nvp-c-install arg nil 'irony))))
      (irony
       ;; install irony server
       ;; do depends first, also only returns process object on windows
       ;; currently
       (if (not (require 'irony nil t))
           (nvp-log "Error: `irony' not installed")
         (nvp-with-gnu/w32
             (nvp-c-install-irony)
           (unless (file-exists-p irony-server-install-prefix)
             (nvp-c-install-irony irony-server-install-prefix)))))
      (t (nvp-c-install arg 'includes)))))

;;; Cache system include paths
;; regen includes after number of days or force with ARG
(defun nvp-c-install-includes (&optional arg)
  (let ((includes (expand-file-name "script/define-includes" nvp-c--dir)))
    (when (or (not (file-exists-p includes))
              (or arg (nvp-file-older-than-days includes 20)))
      (nvp-with-process "bash"
        :proc-name "define-includes"
        :proc-args (includes "make_sys_includes")))))

;;; Irony server

(nvp-with-gnu
  (defun nvp-c-install-irony ()
    (if (not (require 'irony nil t))
        (nvp-log "Error: `irony' not installed")
      (call-interactively 'irony-install-server))))

(nvp-with-w32
  ;; Install irony server using MSYS compilers. Return process object
  (defun nvp-c-install-irony (&optional irony-prefix irony-dir build-cmd)
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
       "cmake" "*nvp-install*" (format "cmake %s && %s" args build-cmd)))))

(provide 'nvp-c-install)
;;; nvp-c-install.el ends here
