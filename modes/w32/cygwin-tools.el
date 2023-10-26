;;; cygwin-tools ---  -*- lexical-binding: t; -*- 
(eval-when-compile
  (require 'cl-lib))
(require 'w32-tools)
(autoload 'nvp-log "nvp-log")
(autoload 'cygwin-mount-activate "cygwin-mount")
(autoload 'config-tools-read-conf "config-tools")

(defvar cygwin-tools-conf)
(when load-file-name
  (setq cygwin-tools-conf
        (expand-file-name "conf/cygwin" (file-name-directory load-file-name))))

(defvar cygwin-tools-rcfiles
  (or (and (bound-and-true-p nvp/config)
           (expand-file-name "bash" nvp/config))
      (expand-file-name "~/.emacs.d/etc/config/bash")))

;; ------------------------------------------------------------

;; uri=https://cygwin.com/setup-x86_64.exe
;; exe=cygwin-setup.exe
;; setup=c:\dev\cygwin-setup.exe
;; rootdir=c:\cygwin
;; localdir=c:\cygwin\packages
;; site=http://mirrors.kernel.org/sourceware/cygwin/
;; packages=default

;; %setup% -q -D -L -d -g -o -s %site% -l %localdir% -R %rootdir% -C Base -P %packages% >> cygwin.log

(defun cygwin-tools-install-setup ()
  (let* ((conf (config-tools-read-conf
                (expand-file-name "cygwin.conf" cygwin-tools-conf)))
         (uri (cdr (assoc-string "uri" conf)))
         (setup (cdr (assoc-string "setup" conf))))
    (when (file-exists-p setup)
      (nvp-log "Deleting old cygwin-setup")
      (delete-file setup t))
    (start-process "cygwin-install" "*nvp-install*" "curl" "-L" "-o" setup uri)))

(defvar cygwin-tools--pending-packages nil)
(defun cygwin-tools-package-sentinel (p m)
  (nvp-log (format "%s: %s" (process-name p) m))
  (when (eq 0 (process-exit-status p))
    (cygwin-tools-install-package cygwin-tools--pending-packages)
    (setq cygwin-tools--pending-packages nil)))

;; ------------------------------------------------------------
;;* Install cygwin packages

;; With no prefix prompt for list of packages, single prefix prompt for file name
;; with list of packages, with double prefix prompt for category to install 
;; (ie. Base, Devel, etc).
;;;###autoload
(defun cygwin-tools-install-package (arg &optional packages file)
  (interactive "P")
  (let* ((args  (and (equal arg '(16))
                     (read-from-minibuffer "Category: " "Base")))
         (packages (or args
                       packages
                       (and (or file (equal arg '(4)))
                            (w32-tools-read-lines
                             (or (and current-prefix-arg
                                      (read-file-name
                                       "Package file: "
                                       cygwin-tools-conf))
                                 file)))
                       (split-string
                        (read-from-minibuffer "Packages to install: "))))
         (params (or args (mapconcat 'identity packages ",")))
         (conf (config-tools-read-conf
                (expand-file-name "cygwin.conf" cygwin-tools-conf)))
         (setup (cdr (assoc-string "setup" conf))))
    (if (not (file-exists-p setup))
        (progn
          (setq cygwin-tools--pending-packages packages)
          (nvp-log (format "Installing cygwin setup to %s" setup))
          (set-process-sentinel (cygwin-tools-install-setup)
                                #'cygwin-tools-package-sentinel))
      (nvp-log (format "Installing: %s" params))
      (funcall 'start-process "cygwin-install" "*nvp-install*"
               setup "-q" "-D" "-L" "-d" "-g" "-o"
               "-s" (cdr (assoc-string "site" conf))
               "-l" (cdr (assoc-string "localdir" conf))
               "-R" (cdr (assoc-string "rootdir" conf))
               (if args "-C" "-P") params))))

;; ------------------------------------------------------------
;;* Link

;; create cygwin link 
(cl-defun cygwin-tools-make-link (&optional
                                  &key link args description target-path icon wd)
  (w32-tools-create-link :link (or link (expand-file-name "cygwin.lnk"
                                                          w32-tools--dir))
                         :args (or args "-i /Cygwin.ico /bin/bash --login")
                         :description (or description "cygwin shell console")
                         :target-path (or target-path
                                          (expand-file-name "bin/mintty.exe"
                                                            (getenv "CYGWIN_HOME")))
                         :icon (or icon (expand-file-name "Cygwin.ico"
                                                          (getenv "CYGWIN_HOME")))
                         :wd (or wd (getenv "HOME"))))

;; ------------------------------------------------------------
;;* terms on windows

;; convert C:/ style path to /cygdrive/c/
(defun cygwin-tools-convert-path (path &optional mount)
  (replace-regexp-in-string "^\\([A-Za-z]\\):"
                            (lambda (x) (concat (or mount "/cygdrive/") (match-string 1 x)))
   (replace-regexp-in-string "\\\\+" "/" path)))

;; Run cygwin bash in native emacs:
;; requires fakecygpty
;;;###autoload
(defun cygwin-tools-bash ()
  "Start `bash' shell."
  (interactive)
  (cygwin-mount-activate)
  (fakecygpty-activate)
  (let ((shell-file-name "bash")
        (explicit-shell-file-name
         (expand-file-name "bin/bash.exe" (getenv "CYGWIN_HOME")))
        (explicit-shell-args '("--login" "-i"))
        ;; (w32-quote-process-args ?\")
        ;; (binary-process-input t)
        ;; (binary-process-output nil)
        )
    (shell)))

;; ensure path conversion / fakecygpty prior to ansi-term
;;;###autoload
(defadvice ansi-term (around cygwin-pty-activate activate)
  (if (not (eq system-type 'windows-nt))
      ad-do-it
    (cygwin-mount-activate)
    (fakecygpty-activate)
    (interactive
     (list
      (cygwin-tools-convert-path
       (expand-file-name "bin/bash.exe" (getenv "CYGWIN_HOME")))
      (ad-get-arg 1)))
    ad-do-it))

;; ------------------------------------------------------------
;;* External shell

;;;###autoload
(defun cygwin-tools-external-shell (arg)
  "Start cygwin shell, with ARG prompt for loading PROFILE settings."
  (interactive "P")
  (if arg
      (let* ((profile
              (ido-completing-read
               "Settings: " 
               (directory-files cygwin-tools-rcfiles nil "^[^.]")))
             (launcher (expand-file-name "cygwin.bat" w32-tools--dir))
             (dotfile (expand-file-name profile cygwin-tools-rcfiles)))
        (call-process-shell-command
         (concat launcher " " (getenv "CYGWIN_HOME") " " dotfile) nil 0))
    (w32-shell-execute "runas"
     (expand-file-name "Cygwin.bat" (getenv "CYGWIN_HOME")))))

(provide 'cygwin-tools)
