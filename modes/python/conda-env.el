;;; conda-env.el --- manage conda environments -*- lexical-binding: t; -*-
;;
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp/modes/python
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'python nil t)
(nvp:decls :p (python pythonic))

(defvar python-shell-virtualenv-root)

;; conda envs directory
(defvar conda-env-home (expand-file-name "miniconda3" (getenv "HOME")))

;; if non-nil be vocal about environment changes
(defvar conda-env-verbose t)

;; hook run after conda environment is activated/deactivated
(defvar conda-env-after-activate-hook ())

;; set current major version after de/activation
(defvar conda-env-python-major-version nil)

;; return the current conda environment associated with this file
(defun conda-env-current-env ()
  (cl-block nil
    (dolist (path (directory-files
                   (expand-file-name "envs" conda-env-home) t "^."))
      (and (cl-member path exec-path :test 'string-prefix-p)
           (cl-return (file-name-nondirectory path))))))

;; remove conda paths and optionally prepend new environment directory
(defun conda-env-update-path (&optional env)
  (let* ((envp (expand-file-name "envs" conda-env-home))
         (path
          (cl-remove-if
           #'(lambda (s) (string-match-p envp s))
           (split-string (getenv "PATH") path-separator))))
    (if env (cons (expand-file-name env envp) path)
      path)))

;; activate whatever conda env is currently active in emacs in comint buffers
(defun conda-env-comint-hook ()
  (when (eq major-mode 'shell-mode)
    (when-let* ((proc (get-buffer-process (current-buffer))))
     (process-put proc 'conda-env (getenv "CONDA_ENV"))
     (comint-send-string proc (format "source activate %s\n" (getenv "CONDA_ENV"))))))

;; read an installed environment -- return nil if "root"
(defun conda-env-read-env ()
  (let ((env (completing-read
              "Environment: " (cons "root"
                                    (directory-files
                                     (expand-file-name
                                      "envs" conda-env-home)
                                     nil "^[^.]")))))
    (and (not (string= env "root")) env)))

(defun conda-env-install (env pkg)
  "Install PKG in conda ENV."
  (interactive
   (let* ((env (conda-env-read-env))
          (pkg (read-string (format "Package to install in %s: " (or env "root")))))
     (list env pkg)))
  (and env pkg
       (let ((proc
               (start-process
                "conda" "*conda-install*" "conda" "install" "-n" env pkg)))
         (set-process-filter proc 'comint-output-filter)
         (with-current-buffer (process-buffer proc)
           (comint-mode)
           (pop-to-buffer (current-buffer))))))

;; Toggle conda environment:
;; When ENV is nil, conda environment is deactivated
;; - update `python-shell-virtualenv-root'
;; - update PATH / remove any previous
;; - update VIRTUAL_ENV
;; - update python-shell-* variables
;; Note:
;; on windows executables are in Scripts
;;;###autoload
(defun conda-env-activate (env)
  (interactive (list (conda-env-read-env)))
  (let* ((path (conda-env-update-path env))
         (env-exec (and env (expand-file-name
                             (nvp:with-gnu/w32 "bin" "Scripts")
                             (car path)))))
    (setq exec-path (if env (cons env-exec (cdr path)) path))
    (setenv "PATH" (mapconcat 'identity exec-path path-separator))
    (setenv "NODE_GYP_FORCE_PYTHON"
            (and env (expand-file-name "bin/python" (car path))))
    (setenv "VIRTUAL_ENV" (and env (car path)))
    (setenv "CONDA_ENV" env)
    (setq python-shell-exec-path (and env (list env-exec))
          python-shell-extra-pythonpaths (and env (list (car path)))
          python-shell-virtualenv-root (and env (car path)))
    (if (not env)
        (progn (and (fboundp 'pythonic-deactivate) (pythonic-deactivate))
               (remove-hook 'comint-mode-hook #'conda-env-comint-hook))
      (add-hook 'comint-mode-hook #'conda-env-comint-hook)
      (and (fboundp 'pythonic-activate) (pythonic-activate (car path))))
    (setq conda-env-python-major-version
          (conda-env-python-major-version "python" t))
    (run-hooks 'conda-env-after-activate-hook)
    (when conda-env-verbose
      (nvp:msg "Conda %sactivated %s" (if env "" "de") (or env "(using root)")))))

;; deactivate conda environment if currently active
(defun conda-env-deactivate ()
  (interactive)
  (conda-env-activate nil))

;;--- Shell Configs --------------------------------------------------

;; return the major version of current python/ipython or nil if none
(defun conda-env-python-major-version (program &optional force)
  (or (and (not force) conda-env-python-major-version)
      (let ((version (ignore-errors
                       (car (process-lines program "--version")))))
        (and version (string-match "[0-9]" version)
             (match-string 0 version)))))

;; setup shell for python/ipython
;; prefer ipython by default, disables native completion for python3
(defun conda-env-setup-shell (arg)
  (interactive "P")
  (let* ((pythons
          (delq nil (mapcar #'(lambda (x) (and (executable-find x) x))
                            '("python" "ipython"))))
         (program (if arg (completing-read "Python: " pythons)
                    (if (member "ipython" pythons) "ipython"
                      (car pythons))))
         (pyver (conda-env-python-major-version "python")))
    (setq python-shell-interpreter program)
    (pcase pyver
      (`"3" (setq python-shell-completion-native-enable nil))
      (_ (setq python-shell-completion-native-enable t)))
    (pcase program 
      (`"ipython"
       (setq python-shell-interpreter-args
             (nvp:with-gnu/w32
                 "-i --matplotlib=auto --pylab=auto --colors=Linux --simple-prompt"
               "-i --matplotlib=auto --pylab=auto --simple-prompt")))
      (_ (setq python-shell-interpreter-args "-i")))))

;; Send buffer if current process, otherwise prompt for conda env to
;; activate, setup shell, and start it
;;;###autoload
(defun conda-env-send-buffer (arg)
  (interactive "P")
  (if (python-shell-get-process)
      (python-shell-send-buffer)
    (call-interactively 'conda-env-activate)
    (conda-env-setup-shell arg)
    (call-interactively 'run-python)))

(provide 'conda-env)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; conda-env.el ends here
