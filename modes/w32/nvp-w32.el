;;; nvp-w32.el --- completely unused -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-08 20:14:07>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created:  8 March 2019

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))

;; ------------------------------------------------------------
;;; Utilities

(defun nvp-w32-convert-path (str)
  (if (eq system-type 'windows-nt)
      (replace-regexp-in-string "/" "\\\\" str)
    str))

;; ------------------------------------------------------------
;;; Link

;; create link to shell
(defvar nvp-w32-vbs-link
  (eval-when-compile
    (mapconcat 'identity
               '("Set sh = CreateObject(\"WScript.Shell\")"
                 "Set link = sh.CreateShortcut(\"%s\")"
                 "link.Arguments = \"%s\"" ;; -i /icon /usr/bin/bash --login
                 "link.Description = \"%s\""
                 "link.TargetPath = \"%s\"" ;; path to mintty.exe
                 "link.WindowStyle = 1"
                 "link.IconLocation = \"%s\""
                 "link.WorkingDirectory = \"%s\""
                 "link.Save")
               "\n")))

(cl-defun nvp-w32-create-link (&key link target-path args
                                    (description "")
                                    (icon "")
                                    (wd ""))
  (let ((script
         (format nvp-w32-vbs-link
                 (nvp-w32-convert-path link)
                 args description
                 (nvp-w32-convert-path target-path)
                 (nvp-w32-convert-path icon)
                 (nvp-w32-convert-path wd)))
        (tmp (concat (make-temp-name "link") ".vbs")))
    (with-temp-file tmp
      (insert script))
    (call-process "cscript" nil nil nil "/nologo" tmp)
    (delete-file tmp)))

;; ------------------------------------------------------------
;;; Shells

;; Launch powershell as admin.
(defun nvp-w32-powershell-admin ()
  (interactive)
  (unless (eq system-type 'windows-nt)
    (user-error "This only works on windows."))
  (w32-shell-execute "runas" "powershell"))

;; Launch the cmdproxy.exe, resetting `SHELL' env variable if it was 
;; changed.
(defun nvp-w32-cmd-proxy ()
  (interactive)
  (let* ((cmd (car (directory-files-recursively
                    (expand-file-name "libexec/emacs/"(getenv "EMACS_DIR"))
                    "cmdproxy.exe")))
         (explicit-shell-file-name cmd)
         (shell-file-name cmd))
    (setenv "SHELL" cmd)
    (shell)))

;; launch admin shell
;; default to opening in current default directory
(defun nvp-w32-cmd-admin (arg)
  (interactive "P")
  (let ((pars (if arg
                  (read-from-minibuffer "Parameters: " "/C &pause")
                (format "/K \"%s && cd %s\""
                        (substring (file-truename default-directory)
                                   0 2)
                        default-directory))))
    (w32-shell-execute "runas" "cmd.exe" pars)))

;; ------------------------------------------------------------
;;; fakecygpty

;; build/setup fakecygpty

; (defun nvp-w32-fakecygpty-sentinel (p m)
;   (nvp-log (format "%s: %s" (process-name p) m))
;   (when (eq 0 (process-exit-status p))
;     (if (string= "install" (process-name p))
;         (nvp-w32-fakecygpty nil t)
;       (nvp-w32-fakecygpty t))))

; (defun nvp-w32-fakecygpty (&optional install final)
;   (let* ((conf (config-tools-read-conf
;                 (expand-file-name "conf/fakecygpty.conf" nvp-w32--dir)))
;          (dir (cdr (assoc-string "directory" conf)))
;          (cmd (cdr (assoc-string "install" conf))))
;     (cond
;      (install
;       (if (not (file-exists-p (expand-file-name "fakecygpty.exe" dir)))
;           (set-process-sentinel
;            (start-process "install" "*w32-tools*"
;                           (expand-file-name "bin/mintty.exe" (getenv "CYGWIN_HOME"))
;                           "/bin/bash" "--login" "-c" cmd)
;            #'nvp-w32-fakecygpty-sentinel)
;         (nvp-w32-fakecygpty nil t)))
;      ;; copy over cygwin1.dll, setup load-path
;      (final
;       (let ((default-directory dir))
;         ;; (unless (file-exists-p "cygwin1.dll")
;         ;;   (copy-file (expand-file-name "bin/cygwin1.dll" (getenv "CYGWIN_HOME"))
;         ;;              "cygwin1.dll"))
;         (unless (file-exists-p (expand-file-name "fakecygpty.el"
;                                                  nvp-w32--dir))
;           (copy-file "fakecygpty.el" (expand-file-name "fakecygpty.el"
;                                                        nvp-w32--dir))))
;       (nvp-path-exec-add (expand-file-name "bin" (getenv "CYGWIN_HOME")))
;       (nvp-path-exec-add dir))
;      (t
;       (if (file-exists-p dir)
;           (nvp-w32-fakecygpty t)
;         (set-process-sentinel
;          (start-process "clone" "*nvp-install*" "git" "clone"
;                         (cdr (assoc-string "repo" conf)) dir)
;          #'nvp-w32-fakecygpty-sentinel))))))

(provide 'nvp-w32)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-w32.el ends here
